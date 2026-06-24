import { useMemo, useState, type ChangeEvent, type ReactNode } from "react";
import * as XLSX from "xlsx";
import mlumrLogoUrl from "./assets/mlumr-logo.svg";
import { parseCsv } from "./mlumr/csv";
import { buildMlumrData } from "./mlumr/data";
import { addIntegration } from "./mlumr/integration";
import { resolveLink } from "./mlumr/links";
import { familyConfig, modelNameFor, supportedFamilies, survivalModelName } from "./mlumr/registry";
import { SURVIVAL_DISTRIBUTIONS } from "./mlumr/survivalDist";
import {
  allStanSummaryRows,
  diagnosticChecks,
  familyAwarePlotVariables,
  familyAwareReportSummaryRows,
  formatNumber,
  type DiagnosticCheck,
  type DrawSummaryRow,
  type DrawVariable
} from "./mlumr/results";
import {
  familyLabel,
  forestMeasures,
  importantPlotVariables,
  linkScaleLabel,
  measureKey,
  measureLabel,
  measureNull,
  modelLabel,
  resultScaleNote,
  variableDetail,
  variableLabel
} from "./mlumr/reporting";
import {
  defaultPriorAux,
  defaultPriorBeta,
  defaultPriorIntercept,
  defaultPriorSigma,
  defaultPriorSmooth
} from "./mlumr/priors";
import { buildStanData, stanDataJson, type BuildStanDataOptions } from "./mlumr/stanData";
import type {
  ColumnMapping,
  CovariateSpec,
  Family,
  LinkName,
  ModelType,
  PriorSpec,
  SamplingOptions,
  StanRun
} from "./mlumr/types";
import type { SurvivalDistribution } from "./mlumr/survivalDist";
import { exampleForFamily } from "./examples/examples";
import { runMlumrModel } from "./runtime/runMlumr";

type PreparedState = {
  modelNames: Partial<Record<ModelType, string>>;
  // Per-model Stan data: the SPFA and relaxed designs differ (nB = 2 + n_cov vs
  // 2 + 2 * n_cov), so each variant needs its own data. `stanJson` keeps the
  // first selected variant for the read-only editor preview.
  stanJsonByModel: Partial<Record<ModelType, string>>;
  stanJson: string;
  notes: string[];
};

type EditorTab = "ipd" | "agd" | "distributions" | "stan";
type ResultTab = "report" | "all" | "diagnostics" | "plots" | "console";
type NavTab = "setup" | "results" | "how" | "about";
type Layout = "tabbed" | "onepage";
type ModelRunMode = ModelType | "both";
type Theme = "light" | "dark";

const NAV_TABS: ReadonlyArray<{ id: NavTab; label: string }> = [
  { id: "setup", label: "Setup & data" },
  { id: "results", label: "Results" },
  { id: "how", label: "How it works" },
  { id: "about", label: "About" }
];

const defaultFamily: Family = "binomial";
const defaultExample = exampleForFamily(defaultFamily);
const defaultLayout: Layout = "tabbed";
const modelRunModes: ModelRunMode[] = ["spfa", "relaxed", "both"];

function initialTheme(): Theme {
  if (typeof window === "undefined") return "dark";
  const stored = window.localStorage.getItem("mlumr-theme");
  if (stored === "light" || stored === "dark") return stored;
  return window.matchMedia?.("(prefers-color-scheme: light)").matches ? "light" : "dark";
}

const compactSampling: SamplingOptions = {
  num_chains: 4,
  num_warmup: 300,
  num_samples: 300,
  init_radius: 2,
  seed: 2026
};

// Prior presets implement the Stan "Prior Choice Recommendations" levels,
// applied to the coefficient (beta + comparator) and intercept scales. The
// per-group controls below stay editable; selecting a preset other than
// "custom" fills them. The "mlumr" preset reproduces the R package defaults.
const PRIOR_PRESETS = ["mlumr", "weak1", "weak10", "supervague", "custom"] as const;
type PriorPreset = (typeof PRIOR_PRESETS)[number];

const PRIOR_PRESET_LABELS: Record<PriorPreset, string> = {
  mlumr: "mlumr default (weakly informative)",
  weak1: "Generic weakly informative — normal(0, 1)",
  weak10: "Weakly informative (wide) — normal(0, 10)",
  supervague: "Super-vague — normal(0, 1e6) (not recommended)",
  custom: "Custom"
};

// UI distribution choices. "cauchy" maps to student_t(df = 1) on emit.
const PRIOR_DISTRIBUTIONS = ["normal", "student_t", "cauchy"] as const;

// A prior group as edited in the UI. df is always carried so switching to
// student_t does not lose the value; it is ignored unless distribution is
// student_t (Stan only reads df for dist == 1).
type PriorGroup = { distribution: "normal" | "student_t" | "cauchy"; mean: number; sd: number; df: number };

type PriorSettings = {
  intercept: PriorGroup;
  beta: PriorGroup;
  comparator: PriorGroup;
  sigma: PriorGroup;
  aux: PriorGroup;
  smooth: PriorGroup;
};

function groupFromSpec(spec: PriorSpec): PriorGroup {
  return {
    distribution: "normal",
    mean: spec.mean ?? 0,
    sd: spec.sd ?? 1,
    df: spec.df ?? 5
  };
}

// cauchy is Student-t with df = 1 (matches the R prior_cauchy() wrapper).
function groupToSpec(group: PriorGroup): PriorSpec {
  if (group.distribution === "cauchy") {
    return { distribution: "student_t", mean: group.mean, sd: group.sd, df: 1 };
  }
  if (group.distribution === "student_t") {
    return { distribution: "student_t", mean: group.mean, sd: group.sd, df: group.df };
  }
  return { distribution: "normal", mean: group.mean, sd: group.sd };
}

function defaultPriorSettings(): PriorSettings {
  return {
    intercept: groupFromSpec(defaultPriorIntercept()),
    beta: groupFromSpec(defaultPriorBeta()),
    comparator: groupFromSpec(defaultPriorBeta()),
    sigma: groupFromSpec(defaultPriorSigma()),
    aux: groupFromSpec(defaultPriorAux()),
    smooth: groupFromSpec(defaultPriorSmooth())
  };
}

// Apply a preset to the coefficient-style groups (intercept, beta, comparator).
// sigma/aux/smooth keep their package defaults regardless of preset; they are
// positive-scale parameters where the wiki coefficient levels do not apply.
function settingsForPreset(preset: PriorPreset): PriorSettings {
  const base = defaultPriorSettings();
  if (preset === "mlumr" || preset === "custom") return base;
  const sd = preset === "weak1" ? 1 : preset === "weak10" ? 10 : 1e6;
  const coef: PriorGroup = { distribution: "normal", mean: 0, sd, df: 5 };
  return { ...base, intercept: { ...coef }, beta: { ...coef }, comparator: { ...coef } };
}

// Compact prior summary for the results metadata pills, e.g. "N(0, 2.5)",
// "t5(0, 2.5)", "Cauchy(0, 2.5)". `half` prefixes positive-scale priors
// (sigma / aux / smooth) which Stan reads as half-distributions.
function trimPriorNumber(value: number): string {
  if (!Number.isFinite(value)) return String(value);
  if (Math.abs(value) >= 1e4) return value.toExponential(0).replace("e+", "e");
  return String(Number(value.toFixed(3)));
}

function formatPriorGroup(group: PriorGroup, half = false): string {
  const prefix = half ? "half-" : "";
  const loc = trimPriorNumber(group.mean);
  const scale = trimPriorNumber(group.sd);
  if (group.distribution === "cauchy") return `${prefix}Cauchy(${loc}, ${scale})`;
  if (group.distribution === "student_t") return `${prefix}t${trimPriorNumber(group.df)}(${loc}, ${scale})`;
  return `${prefix}N(${loc}, ${scale})`;
}

export function App() {
  const [family, setFamily] = useState<Family>(defaultFamily);
  const [model, setModel] = useState<ModelRunMode>("spfa");
  const [link, setLink] = useState<LinkName>(familyConfig[defaultFamily].defaultLink);
  const [distribution, setDistribution] = useState<SurvivalDistribution>("weibull");
  const [nKnots, setNKnots] = useState(7);
  const [ipdCsv, setIpdCsv] = useState(defaultExample.ipdCsv);
  const [agdCsv, setAgdCsv] = useState(defaultExample.agdCsv);
  const [mapping, setMapping] = useState<ColumnMapping>(defaultExample.mapping);
  const [covariateJson, setCovariateJson] = useState(JSON.stringify(defaultExample.covariateSpecs, null, 2));
  const [nInt, setNInt] = useState(32);
  const [sampling, setSampling] = useState<SamplingOptions>(compactSampling);
  const [prepared, setPrepared] = useState<PreparedState | null>(null);
  const [runs, setRuns] = useState<Partial<Record<ModelType, StanRun>>>({});
  const [activeResultModel, setActiveResultModel] = useState<ModelType>("spfa");
  const [status, setStatus] = useState("idle");
  const [exampleLabel, setExampleLabel] = useState(defaultExample.label);
  const [log, setLog] = useState<string[]>([]);
  const [chainProgress, setChainProgress] = useState<Record<number, string>>({});
  const [error, setError] = useState<string | null>(null);
  const [editorTab, setEditorTab] = useState<EditorTab>("ipd");
  const [resultTab, setResultTab] = useState<ResultTab>("report");
  const [plotVariableName, setPlotVariableName] = useState<string>("");
  const [running, setRunning] = useState(false);
  const [navTab, setNavTab] = useState<NavTab>("setup");
  const [layout] = useState<Layout>(defaultLayout);
  const [theme, setTheme] = useState<Theme>(() => initialTheme());
  const [priorPreset, setPriorPreset] = useState<PriorPreset>("mlumr");
  const [priors, setPriors] = useState<PriorSettings>(() => defaultPriorSettings());

  const currentLinks = familyConfig[family].links;
  const activeModelType = model === "both" ? activeResultModel : model;
  const run = runs[activeModelType] ?? null;
  const selectedModelName = selectedModelTypes(model)
    .map((modelType) => resolveModelName(family, distribution, modelType))
    .join(" + ");
  const resultState = useMemo(() => {
    if (!run) {
      return {
        reportRows: [] as DrawSummaryRow[],
        allRows: [] as DrawSummaryRow[],
        checks: [] as DiagnosticCheck[],
        plotVariables: [] as DrawVariable[],
        error: null as string | null
      };
    }
    const effectiveLink = family === "survival" ? "log" : link;
    try {
      return {
        reportRows: familyAwareReportSummaryRows(run, family, effectiveLink),
        allRows: allStanSummaryRows(run),
        checks: diagnosticChecks(run),
        plotVariables: familyAwarePlotVariables(run, family, effectiveLink),
        error: null
      };
    } catch (err) {
      return {
        reportRows: [],
        allRows: [],
        checks: [],
        plotVariables: [],
        error: err instanceof Error ? err.message : String(err)
      };
    }
  }, [run, family, link]);
  // Per-model forest rows for SPFA+relaxed overlay (only when both fits exist).
  const forestModelRows = useMemo(() => {
    const types = selectedModelTypes(model).filter((modelType) => runs[modelType]);
    if (types.length <= 1) return [];
    const eff: LinkName = family === "survival" ? "log" : link;
    return types.map((modelType) => ({
      model: modelType,
      label: modelType === "spfa" ? "SPFA" : "Relaxed",
      rows: familyAwareReportSummaryRows(runs[modelType]!, family, eff)
    }));
  }, [runs, model, family, link]);
  const summaryRows = resultState.reportRows;
  const defaultPlotVariable = importantPlotVariables(summaryRows, family)[0] ?? resultState.plotVariables[0]?.name ?? "";
  const selectedPlotVariable = resultState.plotVariables.find((variable) => variable.name === (plotVariableName || defaultPlotVariable)) ??
    resultState.plotVariables.find((variable) => variable.name === defaultPlotVariable);

  function loadExample(nextFamily = family) {
    const example = exampleForFamily(nextFamily);
    setIpdCsv(example.ipdCsv);
    setAgdCsv(example.agdCsv);
    setMapping(example.mapping);
    setCovariateJson(JSON.stringify(example.covariateSpecs, null, 2));
    setExampleLabel(example.label);
    clearRunState("example loaded");
  }

  function changeFamily(nextFamily: Family) {
    setFamily(nextFamily);
    setLink(familyConfig[nextFamily].defaultLink);
    if (nextFamily === "survival") {
      setDistribution("weibull");
      setNKnots(7);
    }
    loadExample(nextFamily);
  }

  function clearRunState(nextStatus = "idle") {
    setPrepared(null);
    setRuns({});
    setLog([]);
    setError(null);
    setPlotVariableName("");
    setStatus(nextStatus);
  }

  function changeMapping(patch: Partial<ColumnMapping>) {
    setMapping((current) => ({ ...current, ...patch }));
    clearRunState("mapping changed");
  }

  function changePriorPreset(preset: PriorPreset) {
    setPriorPreset(preset);
    if (preset !== "custom") setPriors(settingsForPreset(preset));
    clearRunState("prior preset changed");
  }

  // Editing any per-group control implies a custom prior set; flip the preset
  // selector to "custom" so the UI reflects that the values are user-driven.
  function changePriorGroup(group: keyof PriorSettings, patch: Partial<PriorGroup>) {
    setPriors((current) => ({ ...current, [group]: { ...current[group], ...patch } }));
    setPriorPreset("custom");
    clearRunState("prior changed");
  }

  function toggleTheme() {
    setTheme((current) => {
      const next = current === "dark" ? "light" : "dark";
      window.localStorage.setItem("mlumr-theme", next);
      return next;
    });
  }

  function prepareState(): PreparedState {
    const ipdRows = parseCsv(ipdCsv);
    const agdRows = parseCsv(agdCsv);
    const covariateSpecs = JSON.parse(covariateJson) as Record<string, CovariateSpec>;
    if (family !== "survival") resolveLink(family, link);
    const data = addIntegration(
      buildMlumrData(family, ipdRows, agdRows, mapping),
      covariateSpecs,
      { nInt }
    );
    const priorOptions: BuildStanDataOptions = {
      priorIntercept: groupToSpec(priors.intercept),
      priorBeta: groupToSpec(priors.beta),
      priorBetaComparator: groupToSpec(priors.comparator)
    };
    const stanDataOptions: BuildStanDataOptions = family === "survival"
      ? {
          ...priorOptions,
          survival: { distribution, nKnots },
          priorAux: groupToSpec(priors.aux),
          priorSmooth: groupToSpec(priors.smooth)
        }
      : {
          ...priorOptions,
          link,
          priorSigma: groupToSpec(priors.sigma)
        };
    const selected = selectedModelTypes(model);
    const modelNames: Partial<Record<ModelType, string>> = {};
    const stanJsonByModel: Partial<Record<ModelType, string>> = {};
    for (const modelType of selected) {
      modelNames[modelType] = resolveModelName(family, distribution, modelType);
      // Each variant builds its own design block (SPFA vs relaxed nB differ).
      stanJsonByModel[modelType] = stanDataJson(buildStanData(data, { ...stanDataOptions, model: modelType }));
    }
    return {
      modelNames,
      stanJsonByModel,
      stanJson: stanJsonByModel[selected[0]] ?? "",
      notes: [
        `Index: ${data.indexTreatment} (n=${ipdRows.length})`,
        `Comparator: ${data.comparatorTreatment}${
          family === "binomial"
            ? ` (n=${data.agd.reduce((sum, row) => sum + (Number(row.n) || 0), 0)})`
            : family === "survival"
              ? ` (n=${data.agd.length})`
              : ""
        }`,
        `${mapping.covariates.length} covariates`,
        `${nInt} integration points`,
        `${sampling.num_chains} chains`,
        `${sampling.num_warmup} warmup + ${sampling.num_samples} draws`
      ]
    };
  }

  function prepare() {
    try {
      setError(null);
      setRuns({});
      const nextPrepared = prepareState();
      setPrepared(nextPrepared);
      setStatus("stan data prepared");
      setEditorTab("stan");
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setStatus("failed");
    }
  }

  async function runModel() {
    try {
      setError(null);
      setRuns({});
      setLog([]);
      setChainProgress({});
      setRunning(true);
      setNavTab("results");
      const nextPrepared = prepareState();
      setPrepared(nextPrepared);
      const nextRuns: Partial<Record<ModelType, StanRun>> = {};
      const modelTypes = selectedModelTypes(model);
      for (const modelType of modelTypes) {
        const modelName = nextPrepared.modelNames[modelType];
        if (!modelName) throw new Error(`Missing model name for ${modelType}.`);
        setActiveResultModel(modelType);
        setStatus(`loading ${modelType.toUpperCase()}`);
        setLog((prev) => [...prev.slice(-80), `=== ${modelLabel(modelType)} ===`]);
        const stanJsonForModel = nextPrepared.stanJsonByModel[modelType] ?? nextPrepared.stanJson;
        const result = await runMlumrModel(modelName, stanJsonForModel, sampling, {
          onStatus: (message) => setStatus(`${modelType.toUpperCase()}: ${message}`),
          onProgress: (message) => setLog((prev) => [...prev.slice(-80), message]),
          onChainProgress: (id, message) =>
            setChainProgress((prev) => ({ ...prev, [id]: message }))
        });
        nextRuns[modelType] = result;
        setRuns({ ...nextRuns });
      }
      setResultTab("report");
      setPlotVariableName("");
      setStatus(modelTypes.length === 1
        ? `completed in ${nextRuns[modelTypes[0]]?.elapsedSeconds.toFixed(2)}s`
        : `completed ${modelTypes.map((modelType) => `${modelType.toUpperCase()} ${nextRuns[modelType]?.elapsedSeconds.toFixed(2)}s`).join(" + ")}`
      );
    } catch (err) {
      setError(err instanceof Error ? err.message : String(err));
      setStatus("failed");
    } finally {
      setRunning(false);
    }
  }

  const effectiveLinkForDisplay: LinkName = family === "survival" ? "log" : link;
  // Consolidated outcome + scale pill, e.g. "Binary (logit)", "Survival (weibull)".
  const familyShort = family === "binomial" ? "Binary" : family === "normal" ? "Continuous" : family === "survival" ? "Survival" : "Count";
  const outcomeScalePill = `${familyShort} (${family === "survival" ? distribution : link})`;
  const modelPill = model === "both" ? "SPFA + Relaxed" : model === "spfa" ? "SPFA" : "Relaxed";
  const priorPills: string[] = [
    `β₀ ${formatPriorGroup(priors.intercept)}`,
    `β ${formatPriorGroup(priors.beta)}`,
    ...(model === "relaxed" || model === "both" ? [`β comp ${formatPriorGroup(priors.comparator)}`] : []),
    ...(family === "normal" ? [`σ ${formatPriorGroup(priors.sigma, true)}`] : []),
    ...(family === "survival" ? [`aux ${formatPriorGroup(priors.aux, true)}`] : []),
    ...(family === "survival" && (distribution === "mspline" || distribution === "pexp")
      ? [`smooth ${formatPriorGroup(priors.smooth, true)}`]
      : [])
  ];
  const metadataPills = prepared
    ? [...prepared.notes, outcomeScalePill, modelPill, ...priorPills]
    : ["No Stan data prepared yet", outcomeScalePill, modelPill, ...priorPills];
  const resultTitle = prepared?.modelNames[activeModelType] ?? selectedModelName;
  const tabbed = layout === "tabbed";

  const setupPanel = (
    <aside className="panel setup-panel">
      <div className="setup-scroll">
        <CollapsibleSection num="1" eyebrow="Setup" title="Model configuration" hint="Pick the outcome family, model variant, and link. Switching family loads a matching worked example.">
        <div className="field-grid two">
          <SelectInput label="Family" value={family} values={supportedFamilies} onChange={(value) => changeFamily(value as Family)} />
          <SelectInput label="Model" value={model} values={modelRunModes} onChange={(value) => {
            const nextModel = value as ModelRunMode;
            setModel(nextModel);
            if (nextModel !== "both") setActiveResultModel(nextModel);
            clearRunState("model changed");
          }} />
          {family === "survival" ? (
            <>
              <SelectInput label="Distribution" value={distribution} values={SURVIVAL_DISTRIBUTIONS} onChange={(value) => {
                setDistribution(value as SurvivalDistribution);
                clearRunState("distribution changed");
              }} />
              <NumberInput label="M-spline knots" value={nKnots} min={1} onChange={(value) => {
                setNKnots(value);
                clearRunState("knots changed");
              }} />
            </>
          ) : (
            <>
              <SelectInput label="Link" value={link} values={currentLinks} onChange={(value) => {
                setLink(value as LinkName);
                clearRunState("link changed");
              }} />
              <label>
                Runtime
                <select value="browser" disabled>
                  <option value="browser">Browser WASM</option>
                </select>
              </label>
            </>
          )}
        </div>
        </CollapsibleSection>

        <CollapsibleSection step eyebrow="Columns" title="Data contract" hint="The column names in the CSV inputs shown in the data editor.">
        <div className="field-grid">
          <TextInput label="Treatment column" value={mapping.treatment} onChange={(value) => changeMapping({ treatment: value })} />
          {family === "survival" ? (
            <>
              <TextInput label="IPD time" value={mapping.time ?? ""} onChange={(value) => changeMapping({ time: value })} />
              <TextInput label="IPD status (0/1)" value={mapping.status ?? ""} onChange={(value) => changeMapping({ status: value })} />
              <TextInput label="IPD entry time (optional)" value={mapping.entryTime ?? ""} onChange={(value) => changeMapping({ entryTime: value || undefined })} />
              <TextInput label="AgD time" value={mapping.agdTime ?? ""} onChange={(value) => changeMapping({ agdTime: value })} />
              <TextInput label="AgD status (0/1)" value={mapping.agdStatus ?? ""} onChange={(value) => changeMapping({ agdStatus: value })} />
            </>
          ) : (
            <>
              <TextInput label="IPD outcome" value={mapping.outcome} onChange={(value) => changeMapping({ outcome: value })} />
              {family === "poisson" && (
                <TextInput label="IPD exposure" value={mapping.exposure ?? ""} onChange={(value) => changeMapping({ exposure: value })} />
              )}
              {family === "normal" ? (
                <>
                  <TextInput label="AgD mean" value={mapping.agdMean ?? ""} onChange={(value) => changeMapping({ agdMean: value })} />
                  <TextInput label="AgD SE" value={mapping.agdSe ?? ""} onChange={(value) => changeMapping({ agdSe: value })} />
                </>
              ) : (
                <>
                  <TextInput label="AgD events" value={mapping.agdEvents ?? ""} onChange={(value) => changeMapping({ agdEvents: value })} />
                  {family === "binomial" ? (
                    <TextInput label="AgD total" value={mapping.agdTotal ?? ""} onChange={(value) => changeMapping({ agdTotal: value })} />
                  ) : (
                    <TextInput label="AgD exposure" value={mapping.agdExposure ?? ""} onChange={(value) => changeMapping({ agdExposure: value })} />
                  )}
                </>
              )}
            </>
          )}
          <TextInput label="Covariates" value={mapping.covariates.join(", ")} onChange={(value) => changeMapping({ covariates: splitList(value) })} />
        </div>
        </CollapsibleSection>

        <CollapsibleSection step eyebrow="Priors" title="Priors" hint="Prior distributions for the model parameters. Defaults follow the mlumr package (weakly informative, Stan prior-choice wiki). Pick a preset or edit each group.">
        <div className="field-grid">
          <SelectInput
            label="Prior preset"
            value={priorPreset}
            values={PRIOR_PRESETS}
            onChange={(value) => changePriorPreset(value as PriorPreset)}
          />
          <p className="hint" style={{ gridColumn: "1 / -1", margin: 0 }}>{PRIOR_PRESET_LABELS[priorPreset]}</p>
        </div>
        <PriorGroupControls
          label="Intercept prior"
          group={priors.intercept}
          onChange={(patch) => changePriorGroup("intercept", patch)}
        />
        <PriorGroupControls
          label="Coefficient (beta) prior"
          group={priors.beta}
          onChange={(patch) => changePriorGroup("beta", patch)}
        />
        {(model === "relaxed" || model === "both") && (
          <PriorGroupControls
            label="Comparator coefficient prior (relaxed)"
            group={priors.comparator}
            onChange={(patch) => changePriorGroup("comparator", patch)}
          />
        )}
        {family === "normal" && (
          <PriorGroupControls
            label="Residual SD (sigma) prior - half-distribution"
            group={priors.sigma}
            onChange={(patch) => changePriorGroup("sigma", patch)}
          />
        )}
        {family === "survival" && (
          <PriorGroupControls
            label="Auxiliary shape/scale prior - half-distribution"
            group={priors.aux}
            onChange={(patch) => changePriorGroup("aux", patch)}
          />
        )}
        {family === "survival" && (distribution === "mspline" || distribution === "pexp") && (
          <PriorGroupControls
            label="Smoothing SD prior (M-spline/pexp) - half-distribution"
            group={priors.smooth}
            onChange={(patch) => changePriorGroup("smooth", patch)}
          />
        )}
        </CollapsibleSection>

        <CollapsibleSection step eyebrow="Sampling" title="Controls" hint="Integration points for the comparator covariate distribution, plus MCMC chains, warmup, and draws.">
        <div className="field-grid two">
          <NumberInput label="Integration" value={nInt} min={1} onChange={(value) => {
            setNInt(value);
            clearRunState("integration changed");
          }} />
          <NumberInput label="Chains" value={sampling.num_chains} min={1} onChange={(value) => setSampling({ ...sampling, num_chains: value })} />
          <NumberInput label="Warmup" value={sampling.num_warmup} min={0} onChange={(value) => setSampling({ ...sampling, num_warmup: value })} />
          <NumberInput label="Samples" value={sampling.num_samples} min={1} onChange={(value) => setSampling({ ...sampling, num_samples: value })} />
          <NumberInput label="Seed" value={sampling.seed ?? 0} min={0} onChange={(value) => setSampling({ ...sampling, seed: value })} />
        </div>

        </CollapsibleSection>
      </div>

      <div className="setup-footer">
        <p className="action-hint"><strong>Run Model</strong> builds the Stan data and samples in your browser. <strong>Prepare Stan Data</strong> only generates the JSON so you can inspect it in the Stan JSON tab first.</p>
        <div className="strip sample">
          <span className="k">Sample</span>
          <strong>{exampleLabel}</strong>
        </div>
        {error && <div className="error-box">{error}</div>}
        <div className="action-row">
          <button type="button" className="secondary" disabled={running} onClick={() => loadExample()}><span className="step-num">1</span>Load Sample</button>
          <button type="button" className="secondary" disabled={running} onClick={prepare}><span className="step-num">2</span>Prepare Stan</button>
          <button type="button" className="primary" disabled={running} onClick={runModel}>
            {running ? <span className="btn-busy"><span className="spinner" aria-hidden="true" />Running&hellip;</span> : <><span className="step-num">3</span>Run Model</>}
          </button>
        </div>
        {running && (
          <div className="progress" role="progressbar" aria-label="Sampling in progress">
            <span className="progress-bar" />
          </div>
        )}
        {running && Object.keys(chainProgress).length > 0 && (
          <div className="chain-progress" aria-label="Per-chain sampling progress" style={{ display: "flex", flexDirection: "column", gap: 4, marginTop: 8 }}>
            {Object.entries(chainProgress)
              .sort((a, b) => Number(a[0]) - Number(b[0]))
              .map(([id, msg]) => {
                const match = msg.match(/(\d+)\s*\/\s*(\d+)/);
                const pct = match ? Math.round((100 * Number(match[1])) / Number(match[2])) : null;
                return (
                  <div key={id} style={{ display: "flex", alignItems: "center", gap: 8, fontSize: "0.74rem" }}>
                    <span style={{ width: 52, opacity: 0.7 }}>chain {id}</span>
                    <span style={{ flex: 1, height: 6, background: "rgba(127,127,127,0.18)", borderRadius: 3, overflow: "hidden" }}>
                      <span style={{ display: "block", height: "100%", width: pct != null ? `${pct}%` : "0%", background: "var(--accent, #6ea8fe)", transition: "width 0.2s ease" }} />
                    </span>
                    <span style={{ width: 40, textAlign: "right", opacity: 0.7, fontVariantNumeric: "tabular-nums" }}>{pct != null ? `${pct}%` : ""}</span>
                  </div>
                );
              })}
          </div>
        )}
        <div className="strip status">
          <span className="k">Status</span>
          <strong>{status}</strong>
        </div>
        {running && log.length > 0 && (
          <pre className="live-log" aria-label="Live sampler output">{log.slice(-5).join("\n")}</pre>
        )}
      </div>
    </aside>
  );

  const dataPanel = (
    <section className="panel work-panel">
      <div className="panel-top">
        <PanelHeader num="2" eyebrow="Data" title="Data and generated Stan JSON" hint="Edit the CSV inputs and comparator distributions; the Stan JSON tab shows what gets sent to the sampler." />
      </div>
      <div className="tab-row" role="tablist" aria-label="Data editors">
        <TabButton active={editorTab === "ipd"} onClick={() => setEditorTab("ipd")}>IPD</TabButton>
        <TabButton active={editorTab === "agd"} onClick={() => setEditorTab("agd")}>AgD</TabButton>
        <TabButton active={editorTab === "distributions"} onClick={() => setEditorTab("distributions")}>Distributions</TabButton>
        <TabButton active={editorTab === "stan"} onClick={() => setEditorTab("stan")}>Stan JSON</TabButton>
      </div>
      <p className="editor-hint">{editorHint(editorTab)}</p>
      {editorTab === "ipd" && <CsvEditor label="Individual patient data CSV" value={ipdCsv} onChange={(value) => {
        setIpdCsv(value);
        clearRunState("ipd changed");
      }} />}
      {editorTab === "agd" && <CsvEditor label="Aggregate data CSV" value={agdCsv} onChange={(value) => {
        setAgdCsv(value);
        clearRunState("agd changed");
      }} />}
      {editorTab === "distributions" && <DistributionsEditor value={covariateJson} onChange={(value) => {
        setCovariateJson(value);
        clearRunState("distributions changed");
      }} />}
      {editorTab === "stan" && <StanJsonEditor stanJson={prepared?.stanJson ?? ""} />}
    </section>
  );

  const resultPanel = (
    <section className="panel result-panel">
      <div className="panel-top">
        <PanelHeader num="3" eyebrow="Output" title={resultTitle} hint="Posterior summaries, convergence diagnostics, and plots from the browser Stan fit." />
        <div className="result-actions">
          {tabbed && (
            <span className="run-status">
              {!running && run && <span className="dot-ok" aria-hidden="true" />}
              {running ? "sampling…" : status}
            </span>
          )}
          {tabbed && (
            <button type="button" className="secondary" disabled={running} onClick={runModel}>
              {running ? "Running…" : "Re-run"}
            </button>
          )}
          {model === "both" && (runs.spfa || runs.relaxed) && (
            <label className="active-fit">
              Active fit
              <select value={activeModelType} onChange={(event) => setActiveResultModel(event.target.value as ModelType)}>
                {(["spfa", "relaxed"] as ModelType[]).map((modelType) => (
                  <option key={modelType} value={modelType} disabled={!runs[modelType]}>
                    {modelType.toUpperCase()}
                  </option>
                ))}
              </select>
            </label>
          )}
          {(run || runs.spfa || runs.relaxed) && (
            <button
              type="button"
              className="secondary"
              onClick={() => downloadJson("mlumr-app-draws.json", model === "both" ? runs : run)}
            >
              Download Draws
            </button>
          )}
        </div>
      </div>
      <div className="run-metadata">
        {metadataPills.map((item) => (
          <span className="pill" key={item}><span className="dot" />{item}</span>
        ))}
      </div>

      <div className="tab-row result-tabs" role="tablist" aria-label="Result views">
        <TabButton active={resultTab === "report"} onClick={() => setResultTab("report")}>Report</TabButton>
        <TabButton active={resultTab === "all"} onClick={() => setResultTab("all")}>All Stan Variables</TabButton>
        <TabButton active={resultTab === "diagnostics"} onClick={() => setResultTab("diagnostics")}>Diagnostics</TabButton>
        <TabButton active={resultTab === "plots"} onClick={() => setResultTab("plots")}>Plots</TabButton>
        <TabButton active={resultTab === "console"} onClick={() => setResultTab("console")}>Console</TabButton>
      </div>

      {running && log.length > 0 && (
        <pre className="live-log results" aria-label="Live sampler output">{log.slice(-5).join("\n")}</pre>
      )}

      {resultState.error ? (
        <div className="empty-state">
          <UnanchoredArt />
          <h4>Could not summarize draws</h4>
          <p>{resultState.error}</p>
        </div>
      ) : resultTab === "report" ? (
        <ReportView
          rows={summaryRows}
          family={family}
          link={effectiveLinkForDisplay}
          covariates={mapping.covariates}
          running={running}
          comparison={model === "both" ? buildComparisonRows(runs, family, effectiveLinkForDisplay) : []}
        />
      ) : resultTab === "all" ? (
        <AllResultsView rows={resultState.allRows} family={family} link={effectiveLinkForDisplay} covariates={mapping.covariates} running={running} />
      ) : resultTab === "diagnostics" ? (
        <DiagnosticsView checks={resultState.checks} samplerRows={resultState.allRows.filter((row) => row.group === "Sampler diagnostics")} family={family} link={effectiveLinkForDisplay} covariates={mapping.covariates} running={running} />
      ) : resultTab === "plots" ? (
        <PlotsView
          rows={summaryRows}
          variables={resultState.plotVariables}
          selected={selectedPlotVariable}
          selectedName={plotVariableName || defaultPlotVariable}
          family={family}
          link={effectiveLinkForDisplay}
          covariates={mapping.covariates}
          onSelect={setPlotVariableName}
          running={running}
          numChains={sampling.num_chains}
          modelRows={forestModelRows}
        />
      ) : (
        <ConsoleView log={log} run={run} />
      )}
    </section>
  );

  return (
    <div className="app-shell" data-layout={layout} data-theme={theme}>
      <header className="app-header">
        <div className="topbar">
          <div className="mark">
            <span className="mark-logo">
              <img src={mlumrLogoUrl} alt="" />
            </span>
            <div className="mark-text">
              <h1>
                <b>mlumr</b> <span className="reg">Playground</span>
              </h1>
              <p>Offline-capable ML-UMR fitting with browser-hosted Stan models</p>
            </div>
          </div>
          <div className="topbar-right">
            <button
              type="button"
              className="theme-toggle"
              aria-pressed={theme === "dark"}
              aria-label={`Switch to ${theme === "dark" ? "light" : "dark"} theme`}
              onClick={toggleTheme}
            >
              <span className="theme-track" aria-hidden="true">
                <span className="theme-thumb" />
              </span>
              <span>{theme === "dark" ? "Dark" : "Light"}</span>
            </button>
            <span className="local-badge">
              <span className="pulse" aria-hidden="true" />
              Runs locally, no data leaves your machine
            </span>
            {!tabbed && (
              <nav aria-label="Page sections">
                <a href="#analysis">Analysis</a>
                <a href="#how">How it works</a>
                <a href="#about">About</a>
              </nav>
            )}
          </div>
        </div>
        {tabbed && (
          <nav className="app-nav" role="tablist" aria-label="Sections">
            {NAV_TABS.map((tab) => (
              <button
                key={tab.id}
                type="button"
                role="tab"
                aria-selected={navTab === tab.id}
                className={navTab === tab.id ? "app-nav-tab active" : "app-nav-tab"}
                onClick={() => setNavTab(tab.id)}
              >
                {tab.label}
              </button>
            ))}
          </nav>
        )}
      </header>

      {tabbed ? (
        <main className="tab-area">
          {navTab === "setup" && (
            <div className="work-tab setup-grid">
              {setupPanel}
              {dataPanel}
            </div>
          )}
          {navTab === "results" && (
            <div className="work-tab results-tab">
              {resultPanel}
            </div>
          )}
          {navTab === "how" && <HowItWorksPage />}
          {navTab === "about" && <AboutPage />}
        </main>
      ) : (
        <main>
          <section id="analysis" className="analysis-band">
            <div className="analysis-grid">
              {setupPanel}
              {dataPanel}
              {resultPanel}
            </div>
          </section>
          <InfoSections />
        </main>
      )}
    </div>
  );
}

/* ---------- icons and empty/loading art ---------- */

function InfoGlyph() {
  return (
    <svg width="16" height="16" viewBox="0 0 16 16" fill="none" aria-hidden="true">
      <circle cx="8" cy="8" r="6.4" stroke="currentColor" strokeWidth="1.4" />
      <line x1="8" y1="7" x2="8" y2="11.5" stroke="currentColor" strokeWidth="1.4" strokeLinecap="round" />
      <circle cx="8" cy="4.6" r="0.95" fill="currentColor" />
    </svg>
  );
}

// Empty-state art echoing the broken-anchor motif: two interval marks that do not
// connect, an unanchored comparison waiting to be run.
function UnanchoredArt() {
  return (
    <svg width="120" height="72" viewBox="0 0 120 72" fill="none" className="art" aria-hidden="true">
      <line x1="14" y1="26" x2="50" y2="26" stroke="currentColor" strokeWidth="2.4" strokeLinecap="round" />
      <circle cx="34" cy="26" r="4.5" fill="currentColor" />
      <line x1="70" y1="46" x2="106" y2="46" stroke="currentColor" strokeWidth="2.4" strokeLinecap="round" />
      <circle cx="86" cy="46" r="4.5" fill="none" stroke="currentColor" strokeWidth="2.4" />
      <path d="M52 26 L58 36 M62 36 L68 46" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" strokeDasharray="2 4" opacity="0.7" />
    </svg>
  );
}

function EmptyState({ title, body }: { title: string; body: string }) {
  return (
    <div className="empty-state">
      <UnanchoredArt />
      <h4>{title}</h4>
      <p>{body}</p>
    </div>
  );
}

function LoadingResults() {
  return (
    <div className="result-stack">
      <div className="diagnostic-grid">
        {[0, 1, 2, 3].map((index) => (
          <div className="diagnostic-card na" key={index}>
            <div className="skeleton-stack">
              <span className="skeleton" style={{ width: "45%", height: "0.7rem" }} />
              <span className="skeleton" style={{ width: "60%", height: "1.4rem" }} />
              <span className="skeleton" style={{ width: "90%", height: "0.7rem" }} />
            </div>
          </div>
        ))}
      </div>
      <div className="skeleton-stack">
        {[0, 1, 2, 3, 4, 5].map((index) => <span className="skeleton" key={index} style={{ width: `${90 - index * 6}%` }} />)}
      </div>
    </div>
  );
}

function HeroCards({
  rows,
  family,
  link,
  covariates
}: {
  rows: DrawSummaryRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
}) {
  const note = resultScaleNote(family, link);

  // Survival: collapse the 4 per-population boxes into 2 comparison cards.
  // The log effect is back-transformed (exp -> hazard / time ratio); each card
  // shows both populations side by side.
  if (family === "survival") {
    const find = (name: string) => rows.find((row) => row.variable === name);
    const dIdx = find("delta_index");
    const dCmp = find("delta_comparator");
    const rIdx = find("rmst_diff_index");
    const rCmp = find("rmst_diff_comparator");
    if (!dIdx && !rIdx) return null;
    const collapsed = !!dIdx && !!dCmp && Math.abs(dIdx.mean - dCmp.mean) < 1e-6;
    const popCell = (label: string, value: number | undefined, lo?: number, hi?: number) => (
      <div style={{ display: "flex", flexDirection: "column", gap: 2, flex: 1, minWidth: 0 }}>
        <span style={{ fontSize: "0.7rem", opacity: 0.6, textTransform: "uppercase", letterSpacing: "0.05em" }}>{label}</span>
        <span className="hero-value" style={{ fontSize: "1.7rem" }}>{value != null ? formatNumber(value) : "NA"}</span>
        <span className="hero-ci">{value != null ? `[${formatNumber(lo as number)}, ${formatNumber(hi as number)}]` : "NA"}</span>
      </div>
    );
    return (
      <div className="hero-grid">
        {dIdx && dCmp && (
          <article className="hero-card" style={{ gridColumn: "span 2" }}>
            <span className="hero-label">Hazard ratio (or time-ratio) &mdash; back-transformed from the log-scale effect</span>
            <div style={{ display: "flex", gap: 28 }}>
              {popCell("Index population", Math.exp(dIdx.mean), Math.exp(dIdx.q2_5), Math.exp(dIdx.q97_5))}
              {popCell("Comparator population", Math.exp(dCmp.mean), Math.exp(dCmp.q2_5), Math.exp(dCmp.q97_5))}
            </div>
            {collapsed && (
              <span className="hero-note" style={{ display: "block", fontSize: "0.74rem", opacity: 0.78, marginTop: 8, lineHeight: 1.4 }}>
                Identical in both populations by construction, not because the hazard ratio is collapsible (it is not). This scalar is the start-of-follow-up (t&rarr;0) log hazard ratio, equal to the conditional contrast mu_index &minus; mu_comparator. Under SPFA both treatments share the covariate effect, so the covariate distribution cancels (the log-sum-exp term is common to the index and comparator arms) and the scalar does not depend on the population. The hazard ratio is genuinely non-collapsible over time: the time-varying log-HR (<code>predict(type="loghr")</code>) and the RMST difference below do differ by population.
              </span>
            )}
            <span className="hero-code" title={note}>exp(delta_index) &middot; exp(delta_comparator)</span>
          </article>
        )}
        {rIdx && rCmp && (
          <article className="hero-card" style={{ gridColumn: "span 2" }}>
            <span className="hero-label">RMST difference (index &minus; comparator, time units)</span>
            <div style={{ display: "flex", gap: 28 }}>
              {popCell("Index population", rIdx.mean, rIdx.q2_5, rIdx.q97_5)}
              {popCell("Comparator population", rCmp.mean, rCmp.q2_5, rCmp.q97_5)}
            </div>
            <span className="hero-note" style={{ display: "block", fontSize: "0.74rem", opacity: 0.78, marginTop: 8, lineHeight: 1.4 }}>
              RMST integrates the survival curves over time, so it is non-collapsible and differs between the index and comparator populations.
            </span>
            <span className="hero-code" title={note}>rmst_diff_index &middot; rmst_diff_comparator</span>
          </article>
        )}
      </div>
    );
  }

  const primaryNames = importantPlotVariables(rows, family);
  const heroRows = primaryNames
    .map((name) => rows.find((row) => row.variable === name))
    .filter((row): row is DrawSummaryRow => row !== undefined);
  if (heroRows.length === 0) return null;
  return (
    <div className="hero-grid">
      {heroRows.map((row) => (
        <article className="hero-card" key={row.variable}>
          <span className="hero-label">{variableLabel(row.variable, family, link, covariates)}</span>
          <span className="hero-value">{formatNumber(row.mean)}</span>
          <span className="hero-ci" title={note}>[{formatNumber(row.q2_5)}, {formatNumber(row.q97_5)}]</span>
          <span className="hero-code">{row.variable}</span>
        </article>
      ))}
    </div>
  );
}

const workflowSteps = [
  {
    badge: "1",
    label: "Setup",
    title: "Define the comparison",
    body: "Choose the outcome family, model specification, link function, and runtime mode.",
    meta: "Family, SPFA or relaxed, link"
  },
  {
    badge: "2",
    label: "Data",
    title: "Prepare Stan inputs",
    body: "CSV inputs are standardized into the browser data contract and comparator covariates are turned into integration points.",
    meta: "IPD, AgD, distributions"
  },
  {
    badge: "3",
    label: "Sampling",
    title: "Run WebAssembly Stan",
    body: "The app loads the matching TinyStan bundle and samples locally in a background browser worker.",
    meta: "No server, no local R"
  },
  {
    badge: "4",
    label: "Reporting",
    title: "Inspect model output",
    body: "Treatment effects, population-average predictions, diagnostics, plots, and raw Stan variables are summarized for review.",
    meta: "Tables, plots, diagnostics"
  }
] as const;

// "How it works" workflow. Shared by the tabbed page and one-page info band.
function HowSteps() {
  return (
    <div className="workflow" aria-label="Browser model workflow">
      {workflowSteps.map((step) => (
        <article className="workflow-step" key={step.badge}>
          <div className="workflow-marker" aria-hidden="true">{step.badge}</div>
          <div className="workflow-copy">
            <span className="workflow-label">{step.label}</span>
            <h3>{step.title}</h3>
            <p>{step.body}</p>
            <span className="workflow-meta">{step.meta}</span>
          </div>
        </article>
      ))}
    </div>
  );
}

// Methodology prose. Now shown as a subsection under "How it works".
function MethodologyProse() {
  return (
    <div className="method-grid">
      <article className="method-card">
        <span className="method-label">Data synthesis</span>
        <p>
          ML-UMR combines individual patient data for an index treatment with aggregate data for a comparator treatment. IPD contributes individual likelihood terms, while AgD contributes arm-level summaries after marginalizing over the comparator covariate distribution.
        </p>
      </article>
      <article className="method-card">
        <span className="method-label">Model variants</span>
        <p>
          The SPFA model uses shared prognostic coefficients for both treatments. The relaxed model estimates treatment-specific covariate coefficients, which can represent effect modification but is more weakly identified when aggregate data are sparse.
        </p>
      </article>
      <article className="method-card">
        <span className="method-label">Integration</span>
        <p>
          Comparator-population integration uses deterministic low-discrepancy points with a Gaussian copula correlation adjustment. Normal and Bernoulli covariate margins are supported in this browser build.
        </p>
      </article>
    </div>
  );
}

function AboutContent() {
  return (
    <>
      <div className="about-overview">
        <div className="about-copy">
          <p>
            mlumr Playground is a local, self-contained companion to the mlumr R package. It runs precompiled Stan models in the browser, so no data leaves your machine and no server or local R installation is required.
          </p>
          <p>
            The current build covers binary, continuous, count, and time-to-event outcomes under the SPFA and relaxed specifications, including parametric and flexible-baseline survival variants. Browser runs can also fit SPFA and relaxed models sequentially for side-by-side comparison.
          </p>
          <p className="authors">Created and maintained by <a href="https://orcid.org/0000-0001-6829-0823" target="_blank" rel="noreferrer">Ahmad Sofi-Mahmudi</a> and <a href="https://orcid.org/0000-0002-1365-9002" target="_blank" rel="noreferrer">Conor Chandler</a>.</p>
        </div>
        <div className="about-highlights" aria-label="Application capabilities">
          <article>
            <span>Privacy</span>
            <strong>Browser-local</strong>
            <p>CSV inputs, generated Stan data, sampling, and results stay on the local machine.</p>
          </article>
          <article>
            <span>Backend</span>
            <strong>TinyStan WASM</strong>
            <p>Each model is precompiled and sampled in a background worker for an offline-capable workflow.</p>
          </article>
          <article>
            <span>Coverage</span>
            <strong>Core ML-UMR families</strong>
            <p>Binary, continuous, count, and survival examples are bundled from mlumr-style data.</p>
          </article>
        </div>
      </div>
      <div className="resource-section">
        <div className="section-heading resource-heading">
          <h3>References and resources</h3>
          <span className="count">7 links</span>
        </div>
        <ul className="resource-list">
          <li>
            <span className="resource-kind">Source code</span>
            <a href="https://github.com/choxos/mlumr" target="_blank" rel="noreferrer">mlumr on GitHub</a>
            <p>Package source, documentation sources, and development history.</p>
          </li>
          <li>
            <span className="resource-kind">Documentation</span>
            <a href="https://choxos.github.io/mlumr" target="_blank" rel="noreferrer">mlumr package documentation</a>
            <p>Reference documentation for the R package that the browser app mirrors.</p>
          </li>
          <li>
            <span className="resource-kind">Conference poster</span>
            <a href="https://www.valueinhealthjournal.com/article/S1098-3015(25)05944-3/abstract" target="_blank" rel="noreferrer">Anchors away</a>
            <p>Chandler C, Ishak KJ. ML-UMR for unanchored indirect comparisons. ISPOR Europe 2025.</p>
          </li>
          <li>
            <span className="resource-kind">Survival poster</span>
            <a href="https://www.ispor.org/heor-resources/presentations-database/presentation-cti/ispor-2026/poster-session-3-3/surviving-unanchored-indirect-comparisons-an-extension-of-multilevel-unanchored-meta-regression-ml-umr-for-survival-analyses" target="_blank" rel="noreferrer">Surviving unanchored indirect comparisons</a>
            <p>Chandler C, Ishak KJ. ML-UMR extension for survival analyses. ISPOR 2026.</p>
          </li>
          <li>
            <span className="resource-kind">JOSS article</span>
            <a href="https://doi.org/10.21105/joss.09531" target="_blank" rel="noreferrer">stan-playground paper</a>
            <p>Ward B, Soules J, Magland J. <em>Journal of Open Source Software</em>. 2026;11(118):9531.</p>
          </li>
          <li>
            <span className="resource-kind">WebAssembly backend</span>
            <a href="https://github.com/flatironinstitute/stan-playground" target="_blank" rel="noreferrer">Stan Playground</a>
            <p>Browser-hosted Stan model execution used as the technical reference for this app.</p>
          </li>
          <li>
            <span className="resource-kind">Sampler interface</span>
            <a href="https://github.com/WardBrian/tinystan" target="_blank" rel="noreferrer">TinyStan</a>
            <p>Minimal interface to Stan samplers, including the WebAssembly bundles loaded here.</p>
          </li>
        </ul>
      </div>
    </>
  );
}

// Tabbed-layout pages. "How it works" merges the workflow step cards and the
// methodology prose (steps first, then a methodology subsection).
function HowItWorksPage() {
  return (
    <div className="info-page">
      <div className="section-head">
        <span className="eyebrow">Workflow</span>
        <h2>How It Works</h2>
      </div>
      <HowSteps />
      <div className="info-subsection">
        <div className="section-head">
          <span className="eyebrow">Statistical Model</span>
          <h2>Methodology</h2>
        </div>
        <MethodologyProse />
      </div>
    </div>
  );
}

function AboutPage() {
  return (
    <div className="info-page">
      <div className="section-head">
        <span className="eyebrow">Overview</span>
        <h2>About</h2>
      </div>
      <AboutContent />
    </div>
  );
}

// One-page-layout stacked info bands. "How it works" and "Methodology" are
// merged into a single band (steps first, then a methodology subsection).
function InfoSections() {
  return (
    <>
      <section id="how" className="info-band">
        <div className="info-inner">
          <div className="section-head">
            <span className="eyebrow">Workflow</span>
            <h2>How It Works</h2>
          </div>
          <HowSteps />
          <div className="info-subsection">
            <div className="section-head">
              <span className="eyebrow">Statistical Model</span>
              <h2>Methodology</h2>
            </div>
            <MethodologyProse />
          </div>
        </div>
      </section>

      <section id="about" className="info-band alt">
        <div className="info-inner">
          <div className="section-head">
            <span className="eyebrow">Overview</span>
            <h2>About</h2>
          </div>
          <AboutContent />
        </div>
      </section>
    </>
  );
}

function CollapsibleSection({
  num,
  eyebrow,
  title,
  step = false,
  hint,
  children
}: {
  num?: string;
  eyebrow: string;
  title: string;
  step?: boolean;
  hint?: string;
  children: ReactNode;
}) {
  const [open, setOpen] = useState(true);
  return (
    <section className={step ? "setup-section step" : "setup-section"}>
      <button
        type="button"
        className="section-head"
        aria-expanded={open}
        onClick={() => setOpen((current) => !current)}
      >
        <span className="section-head-row">
          <span className="eyebrow">{num != null && <span className="num">{num}</span>}{eyebrow}</span>
          <span className={open ? "chev open" : "chev"} aria-hidden="true">›</span>
        </span>
        <h2>{title}</h2>
        {hint && <p className="hint">{hint}</p>}
      </button>
      {open && <div className="setup-section-body">{children}</div>}
    </section>
  );
}

function PanelHeader({ num, eyebrow, title, step = false, hint }: { num?: string; eyebrow: string; title: string; step?: boolean; hint?: string }) {
  return (
    <div className={step ? "panel-header step" : "panel-header"}>
      <span className="eyebrow">{num != null && <span className="num">{num}</span>}{eyebrow}</span>
      <h2>{title}</h2>
      {hint && <p className="hint">{hint}</p>}
    </div>
  );
}

function editorHint(tab: EditorTab): string {
  switch (tab) {
    case "ipd":
      return "One row per patient in the index-treatment trial: the treatment, outcome, and covariate columns named in the Data Contract.";
    case "agd":
      return "One row per comparator arm: the aggregate outcome plus covariate summaries (means, and SDs for continuous covariates).";
    case "distributions":
      return "JSON describing each comparator covariate distribution; these build the numerical integration points, e.g. {\"age\": {\"type\": \"normal\", \"mean\": 62, \"sd\": 9}}.";
    case "stan":
      return "Read-only Stan data JSON generated from your inputs by Prepare Stan Data or Run Model.";
  }
}

function ReportView({
  rows,
  family,
  link,
  covariates,
  running,
  comparison
}: {
  rows: DrawSummaryRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
  running: boolean;
  comparison: ComparisonRow[];
}) {
  if (running) return <LoadingResults />;
  if (rows.length === 0) {
    return <EmptyState title="No posterior yet" body="Run the model to populate treatment effects, population-average predictions, and model parameters from the browser Stan fit." />;
  }
  const groups = groupedRows(rows);
  return (
    <div className="result-stack">
      {comparison.length > 0 && <ComparisonTable rows={comparison} family={family} link={link} covariates={covariates} />}
      <HeroCards rows={rows} family={family} link={link} covariates={covariates} />
      <div className="scale-note">
        <InfoGlyph />
        <span>{resultScaleNote(family, link)}</span>
      </div>
      {groups.map(([group, groupRows]) => (
        <section className="result-section" key={group}>
          <div className="section-heading">
            <h3>{group}</h3>
            <span className="count">{groupRows.length} {groupRows.length === 1 ? "row" : "rows"}</span>
          </div>
          <SummaryTable rows={groupRows} family={family} link={link} covariates={covariates} showGroup={false} />
        </section>
      ))}
    </div>
  );
}

type ComparisonRow = {
  variable: string;
  spfa?: DrawSummaryRow;
  relaxed?: DrawSummaryRow;
};

function ComparisonTable({
  rows,
  family,
  link,
  covariates
}: {
  rows: ComparisonRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
}) {
  return (
    <section className="result-section comparison-section">
      <div className="section-heading">
        <h3>SPFA vs Relaxed</h3>
        <span className="count">{rows.length} primary estimands</span>
      </div>
      <div className="table-wrap">
        <table>
          <thead>
            <tr>
              <th>Estimand</th>
              <th className="num">SPFA mean</th>
              <th className="num">SPFA 95% CrI</th>
              <th className="num">Relaxed mean</th>
              <th className="num">Relaxed 95% CrI</th>
            </tr>
          </thead>
          <tbody>
            {rows.map((row) => (
              <tr key={row.variable}>
                <td>
                  <div className="variable-cell">
                    <strong>{variableLabel(row.variable, family, link, covariates)}</strong>
                    <span className="code">{row.variable}</span>
                  </div>
                </td>
                <td className="num">{row.spfa ? formatNumber(row.spfa.mean) : "NA"}</td>
                <td className="num">{row.spfa ? `[${formatNumber(row.spfa.q2_5)}, ${formatNumber(row.spfa.q97_5)}]` : "NA"}</td>
                <td className="num">{row.relaxed ? formatNumber(row.relaxed.mean) : "NA"}</td>
                <td className="num">{row.relaxed ? `[${formatNumber(row.relaxed.q2_5)}, ${formatNumber(row.relaxed.q97_5)}]` : "NA"}</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>
    </section>
  );
}

function AllResultsView({
  rows,
  family,
  link,
  covariates,
  running
}: {
  rows: DrawSummaryRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
  running: boolean;
}) {
  if (running) return <LoadingResults />;
  if (rows.length === 0) {
    return <EmptyState title="Nothing to list" body="Every scalar returned by TinyStan, including generated quantities, parameters, transformed parameters, pointwise log-likelihoods, and sampler diagnostics, appears here after a run." />;
  }
  return (
    <div className="result-stack fill">
      <div className="scale-note">
        <InfoGlyph />
        <span>This table includes every scalar returned by TinyStan: generated quantities, model parameters, transformed parameters, pointwise log likelihoods, and sampler diagnostics. Download draws for the full posterior sample.</span>
      </div>
      <div className="section-heading">
        <h3>All Stan variables</h3>
        <span className="count">{rows.length} rows</span>
      </div>
      <SummaryTable rows={rows} family={family} link={link} covariates={covariates} showGroup />
    </div>
  );
}

function DiagnosticsView({
  checks,
  samplerRows,
  family,
  link,
  covariates,
  running
}: {
  checks: DiagnosticCheck[];
  samplerRows: DrawSummaryRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
  running: boolean;
}) {
  if (running) return <LoadingResults />;
  if (checks.length === 0) {
    return <EmptyState title="No diagnostics yet" body="Convergence checks, including R-hat, effective sample size, divergences, tree depth, and E-BFMI, are computed once the sampler finishes." />;
  }
  return (
    <div className="result-stack">
      <div className="diagnostic-grid">
        {checks.map((check) => (
          <article key={check.metric} className={`diagnostic-card ${check.status}`}>
            <div className="metric"><span>{check.metric}</span><span className="diag-chip" /></div>
            <strong className="val">{check.value}</strong>
            <p>{check.detail}</p>
          </article>
        ))}
      </div>
      {samplerRows.length > 0 && (
        <section className="result-section">
          <div className="section-heading">
            <h3>Sampler Diagnostic Variables</h3>
            <span className="count">{samplerRows.length} rows</span>
          </div>
          <SummaryTable rows={samplerRows} family={family} link={link} covariates={covariates} showGroup={false} />
        </section>
      )}
    </div>
  );
}

function PlotsView({
  rows,
  variables,
  selected,
  selectedName,
  family,
  link,
  covariates,
  onSelect,
  running,
  numChains,
  modelRows
}: {
  rows: DrawSummaryRow[];
  variables: DrawVariable[];
  selected: DrawVariable | undefined;
  selectedName: string;
  family: Family;
  link: LinkName;
  covariates: string[];
  onSelect: (value: string) => void;
  running: boolean;
  numChains: number;
  modelRows?: Array<{ model: ModelType; label: string; rows: DrawSummaryRow[] }>;
}) {
  // Hooks must run before any early return (rules of hooks).
  const [selectedMeasure, setSelectedMeasure] = useState<string>("");

  if (running) return <LoadingResults />;
  if (variables.length === 0 || rows.length === 0) {
    return <EmptyState title="No plots yet" body="Forest, trace, posterior, and rank diagnostics render here once draws are available." />;
  }
  const chainCount = selected ? selected.chains.length : numChains;

  // The distinct measures available, derived from the important plot variables.
  const measures = forestMeasures(rows, family);
  const measureFallback = measures[0] ?? "";
  const activeMeasure = measures.includes(selectedMeasure) ? selectedMeasure : measureFallback;

  // Variables of the active measure (its index- and comparator-population rows),
  // ordered as importantPlotVariables returns them.
  const measureVariables = importantPlotVariables(rows, family).filter(
    (variable) => measureKey(variable) === activeMeasure
  );

  const overlay = (modelRows ?? []).filter((entry) => entry.rows.length > 0);
  const isOverlay = overlay.length > 1;
  const seriesColors = ["var(--accent-deep)", "var(--c2)"];
  const legend = isOverlay
    ? overlay.map((entry, index) => ({ label: entry.label, color: seriesColors[index % seriesColors.length] }))
    : undefined;

  const forestGroups = measureVariables.map((variable) => {
    const points = isOverlay
      ? overlay
          .map((entry, index) => {
            const row = entry.rows.find((candidate) => candidate.variable === variable);
            if (!row) return null;
            return {
              model: entry.model,
              color: seriesColors[index % seriesColors.length],
              mean: row.mean,
              lo: row.q2_5,
              hi: row.q97_5
            };
          })
          .filter((point): point is NonNullable<typeof point> => point !== null)
      : (() => {
          const row = rows.find((candidate) => candidate.variable === variable);
          if (!row) return [];
          return [{ color: "var(--accent-deep)", mean: row.mean, lo: row.q2_5, hi: row.q97_5 }];
        })();
    return {
      variable,
      label: variableLabel(variable, family, link, covariates),
      code: variable,
      points
    };
  }).filter((group) => group.points.length > 0);

  const nullRef = measureNull(activeMeasure);

  return (
    <div className="plot-stack">
      <section className="plot-panel wide">
        <div className="section-heading">
          <h3>Forest, primary estimands</h3>
          <span className="count">{forestGroups.length} {forestGroups.length === 1 ? "row" : "rows"} &middot; 95% CrI</span>
        </div>
        <div className="plot-controls">
          <label>
            Forest measure
            <select value={activeMeasure} onChange={(event) => setSelectedMeasure(event.target.value)}>
              {measures.map((measure) => (
                <option key={measure} value={measure}>
                  {measureLabel(measure, importantPlotVariables(rows, family), family, link, covariates)}
                </option>
              ))}
            </select>
          </label>
        </div>
        <ForestPlot groups={forestGroups} nullRef={nullRef} legend={legend} />
      </section>

      <div className="plot-controls">
        <label>
          Diagnostic variable
          <select value={selectedName} onChange={(event) => onSelect(event.target.value)}>
            {variables.map((variable) => (
              <option key={variable.name} value={variable.name}>
                {variableLabel(variable.name, family, link, covariates)}
              </option>
            ))}
          </select>
        </label>
      </div>

      {selected ? (
        <div className="plot-grid">
          <section className="plot-panel">
            <div className="section-heading">
              <h3>Trace</h3>
              <span className="count">{selected.values.length} draws</span>
            </div>
            <TracePlot variable={selected} />
            <ChainLegend n={chainCount} />
          </section>
          <section className="plot-panel">
            <div className="section-heading">
              <h3>Posterior</h3>
              <span className="count">{selected.chains.length} chains</span>
            </div>
            <HistogramPlot variable={selected} />
          </section>
          <section className="plot-panel wide">
            <div className="section-heading">
              <h3>Rank (uniformity across chains)</h3>
              <span className="count">{variableLabel(selected.name, family, link, covariates)}</span>
            </div>
            <RankPlot variable={selected} />
            <ChainLegend n={chainCount} />
          </section>
        </div>
      ) : (
        <EmptyState title="Select a variable" body="Choose a variable above to draw trace, histogram, and rank diagnostics." />
      )}
    </div>
  );
}

function ConsoleView({ log, run }: { log: string[]; run: StanRun | null }) {
  return (
    <div className="result-stack fill">
      <pre className="console" aria-label="Sampler console">
        {run?.consoleMessages.length ? run.consoleMessages.join("\n") : log.length === 0 ? "Sampler messages will appear here." : log.join("\n")}
      </pre>
    </div>
  );
}

function SummaryTable({
  rows,
  family,
  link,
  covariates,
  showGroup
}: {
  rows: DrawSummaryRow[];
  family: Family;
  link: LinkName;
  covariates: string[];
  showGroup: boolean;
}) {
  return (
    <div className="table-wrap">
      <table>
        <thead>
          <tr>
            {showGroup && <th>Group</th>}
            <th>Variable</th>
            <th className="num">Mean</th>
            <th className="num">MCSE</th>
            <th className="num">SD</th>
            <th className="num">2.5%</th>
            <th className="num">50%</th>
            <th className="num">97.5%</th>
            <th className="num">ESS</th>
            <th className="num">R-hat</th>
          </tr>
        </thead>
        <tbody>
          {rows.map((row) => (
            <tr key={`${row.group}-${row.variable}`}>
              {showGroup && <td>{row.group}</td>}
              <td>
                <div className="variable-cell">
                  <strong>{variableLabel(row.variable, family, link, covariates)}</strong>
                  <span className="code">{row.variable}</span>
                  {variableDetail(row.variable, family, link) && <span className="det">{variableDetail(row.variable, family, link)}</span>}
                </div>
              </td>
              <td className="num">{formatNumber(row.mean)}</td>
              <td className="num">{formatNumber(row.mcse)}</td>
              <td className="num">{formatNumber(row.sd)}</td>
              <td className="num">{formatNumber(row.q2_5)}</td>
              <td className="num">{formatNumber(row.q50)}</td>
              <td className="num">{formatNumber(row.q97_5)}</td>
              <td className="num">{formatNumber(row.ess)}</td>
              <td className="num">{formatNumber(row.rhat)}</td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

/* ---------- charts (handoff visual language) ---------- */

const CHAIN_COLORS = ["var(--c0)", "var(--c1)", "var(--c2)", "var(--c3)"];

type ForestPoint = { model?: ModelType; color: string; mean: number; lo: number; hi: number };
type ForestGroup = { variable: string; label: string; code: string; points: ForestPoint[] };

// Single-measure forest plot. Each group is one population row; within a row,
// each point is a model series (one point in single-series mode, SPFA+relaxed in
// overlay mode) offset vertically and colored distinctly. The x-axis is auto
// scaled to the plotted values only, valid because all rows share one measure.
function ForestPlot({
  groups,
  nullRef,
  legend
}: {
  groups: ForestGroup[];
  nullRef: number | null;
  legend?: Array<{ label: string; color: string }>;
}) {
  if (groups.length === 0) return <EmptyState title="No interval rows" body="No primary estimands are available to plot for this measure." />;
  const width = 900;
  const rowH = 52;
  const left = 330;
  const right = 120;
  const top = 24;
  const bottom = legend ? 52 : 36;
  const height = top + bottom + rowH * groups.length;
  const xs = groups.flatMap((group) => group.points.flatMap((point) => [point.lo, point.hi, point.mean])).filter(Number.isFinite);
  let [min, max] = extent(xs);
  const pad = (max - min) * 0.12 || 0.5;
  min -= pad;
  max += pad;
  const plotW = width - left - right;
  const x = (value: number) => left + ((value - min) / (max - min)) * plotW;
  const ticks = niceTicks(min, max, 5);
  const showNull = nullRef !== null && nullRef > min && nullRef < max;

  return (
    <div className="chart-frame">
      <svg className="chart" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="Posterior interval plot">
        {ticks.map((tick) => (
          <g key={tick}>
            <line x1={x(tick)} x2={x(tick)} y1={top - 6} y2={top + groups.length * rowH} stroke="var(--hairline)" strokeWidth="1" />
            <text x={x(tick)} y={top + groups.length * rowH + 18} textAnchor="middle" fontSize="11" fontFamily="var(--font-mono)" fill="var(--muted)">{fmt(tick)}</text>
          </g>
        ))}
        {showNull && (
          <line x1={x(nullRef!)} x2={x(nullRef!)} y1={top - 6} y2={top + groups.length * rowH} stroke="var(--faint)" strokeWidth="1.2" strokeDasharray="4 4" />
        )}
        {groups.map((group, index) => {
          const rowY = top + index * rowH + rowH / 2;
          const n = group.points.length;
          const spread = Math.min(16, rowH * 0.32);
          return (
            <g key={group.variable}>
              {index > 0 && <line x1={left} x2={width - right} y1={top + index * rowH} y2={top + index * rowH} stroke="var(--hairline)" strokeWidth="1" />}
              <text x={12} y={rowY - 3} fontSize="10" fontWeight="600" fill="var(--ink)">{shortLabel(group.label, 80)}</text>
              <text x={12} y={rowY + 12} fontSize="8.5" fontFamily="var(--font-mono)" fill="var(--muted)">{group.code}</text>
              {group.points.map((point, pointIndex) => {
                const offset = n > 1 ? (pointIndex - (n - 1) / 2) * spread : 0;
                const y = rowY + offset;
                return (
                  <g key={point.model ?? pointIndex}>
                    <line x1={x(point.lo)} x2={x(point.hi)} y1={y} y2={y} stroke={point.color} strokeWidth="3" strokeLinecap="round" />
                    <line x1={x(point.lo)} x2={x(point.lo)} y1={y - 5} y2={y + 5} stroke={point.color} strokeWidth="2" />
                    <line x1={x(point.hi)} x2={x(point.hi)} y1={y - 5} y2={y + 5} stroke={point.color} strokeWidth="2" />
                    <circle cx={x(point.mean)} cy={y} r="5" fill={point.color} stroke="var(--surface)" strokeWidth="1.5" />
                  </g>
                );
              })}
              {n === 1 && (
                <>
                  <text x={width - 14} y={rowY - 2} textAnchor="end" fontSize="12" fontFamily="var(--font-mono)" fontWeight="600" fill="var(--ink)">{fmt(group.points[0].mean)}</text>
                  <text x={width - 14} y={rowY + 12} textAnchor="end" fontSize="10" fontFamily="var(--font-mono)" fill="var(--muted)">[{fmt(group.points[0].lo)}, {fmt(group.points[0].hi)}]</text>
                </>
              )}
            </g>
          );
        })}
        {legend && legend.map((entry, index) => (
          <g key={entry.label} transform={`translate(${left + index * 130}, ${height - 16})`}>
            <line x1={0} x2={22} y1={0} y2={0} stroke={entry.color} strokeWidth="3" strokeLinecap="round" />
            <circle cx={11} cy={0} r="5" fill={entry.color} stroke="var(--surface)" strokeWidth="1.5" />
            <text x={30} y={4} fontSize="11" fontFamily="var(--font-mono)" fill="var(--ink)">{entry.label}</text>
          </g>
        ))}
      </svg>
    </div>
  );
}

function ChartAxes({
  width,
  height,
  pad,
  min,
  max,
  xLabel
}: {
  width: number;
  height: number;
  pad: { top: number; right: number; bottom: number; left: number };
  min: number;
  max: number;
  xLabel?: string;
}) {
  const ticks = niceTicks(min, max, 4);
  const y = (value: number) => height - pad.bottom - ((value - min) / (max - min)) * (height - pad.top - pad.bottom);
  return (
    <g>
      {ticks.map((tick) => (
        <g key={tick}>
          <line x1={pad.left} x2={width - pad.right} y1={y(tick)} y2={y(tick)} stroke="var(--hairline)" strokeWidth="1" />
          <text x={pad.left - 8} y={y(tick) + 3.5} textAnchor="end" fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--muted)">{fmt(tick)}</text>
        </g>
      ))}
      <line x1={pad.left} x2={width - pad.right} y1={height - pad.bottom} y2={height - pad.bottom} stroke="var(--border-strong)" strokeWidth="1" />
      <line x1={pad.left} x2={pad.left} y1={pad.top} y2={height - pad.bottom} stroke="var(--border-strong)" strokeWidth="1" />
      {xLabel && <text x={(pad.left + width - pad.right) / 2} y={height - 6} textAnchor="middle" fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--faint)">{xLabel}</text>}
    </g>
  );
}

function TracePlot({ variable }: { variable: DrawVariable }) {
  const width = 760;
  const height = 240;
  const pad = { top: 16, right: 16, bottom: 30, left: 52 };
  const [min, max] = extent(variable.values);
  const xMax = Math.max(...variable.chains.map((chain) => chain.length), 2) - 1;
  const x = (index: number) => pad.left + (index / xMax) * (width - pad.left - pad.right);
  const y = (value: number) => height - pad.bottom - ((value - min) / (max - min)) * (height - pad.top - pad.bottom);
  return (
    <div className="chart-frame">
      <svg className="chart" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="Trace plot">
        <ChartAxes width={width} height={height} pad={pad} min={min} max={max} xLabel="iteration" />
        {variable.chains.map((chain, chainIndex) => (
          <polyline
            key={chainIndex}
            fill="none"
            stroke={CHAIN_COLORS[chainIndex % 4]}
            strokeWidth="1"
            opacity="0.78"
            points={chain.map((value, index) => `${x(index)},${y(value)}`).join(" ")}
          />
        ))}
      </svg>
    </div>
  );
}

function HistogramPlot({ variable }: { variable: DrawVariable }) {
  const width = 760;
  const height = 240;
  const pad = { top: 16, right: 16, bottom: 30, left: 52 };
  const nBins = Math.max(14, Math.ceil(Math.sqrt(variable.values.length)));
  const bins = makeHistogram(variable.values, nBins);
  const maxCount = Math.max(...bins.map((bin) => bin.count), 1);
  const [vmin, vmax] = extent(variable.values);
  const plotW = width - pad.left - pad.right;
  const plotH = height - pad.top - pad.bottom;
  const barW = plotW / bins.length;
  const xt = niceTicks(vmin, vmax, 5);
  const sx = (value: number) => pad.left + ((value - vmin) / (vmax - vmin)) * plotW;
  const meanX = sx(variable.values.length > 0 ? mean(variable.values) : vmin);
  return (
    <div className="chart-frame">
      <svg className="chart" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="Posterior histogram">
        <line x1={pad.left} x2={width - pad.right} y1={height - pad.bottom} y2={height - pad.bottom} stroke="var(--border-strong)" strokeWidth="1" />
        {bins.map((bin, index) => {
          const h = (bin.count / maxCount) * plotH;
          return <rect key={index} x={pad.left + index * barW + 0.8} y={height - pad.bottom - h} width={Math.max(1, barW - 1.6)} height={h} fill="var(--accent)" opacity="0.82" rx="1.5" />;
        })}
        <line x1={meanX} x2={meanX} y1={pad.top} y2={height - pad.bottom} stroke="var(--accent-deep)" strokeWidth="1.5" strokeDasharray="3 3" />
        {xt.map((tick) => (
          <text key={tick} x={sx(tick)} y={height - pad.bottom + 16} textAnchor="middle" fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--muted)">{fmt(tick)}</text>
        ))}
        <text x={(pad.left + width - pad.right) / 2} y={height - 4} textAnchor="middle" fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--faint)">posterior value</text>
      </svg>
    </div>
  );
}

function RankPlot({ variable }: { variable: DrawVariable }) {
  const width = 760;
  const height = 240;
  const pad = { top: 16, right: 16, bottom: 32, left: 52 };
  const nBins = 22;
  const ranks = rankBins(variable.chains, nBins);
  const maxCount = Math.max(...ranks.flat(), 1);
  const plotW = width - pad.left - pad.right;
  const plotH = height - pad.top - pad.bottom;
  const groupW = plotW / nBins;
  const barW = groupW / Math.max(1, ranks.length);
  return (
    <div className="chart-frame">
      <svg className="chart" viewBox={`0 0 ${width} ${height}`} role="img" aria-label="Rank plot by chain">
        <line x1={pad.left} x2={width - pad.right} y1={height - pad.bottom} y2={height - pad.bottom} stroke="var(--border-strong)" strokeWidth="1" />
        {ranks.map((bins, chainIndex) => bins.map((count, binIndex) => {
          const h = (count / maxCount) * plotH;
          return <rect key={`${chainIndex}-${binIndex}`} x={pad.left + binIndex * groupW + chainIndex * barW + 0.4} y={height - pad.bottom - h} width={Math.max(1, barW - 0.8)} height={h} fill={CHAIN_COLORS[chainIndex % 4]} opacity="0.82" rx="1" />;
        }))}
        <text x={pad.left} y={height - 6} fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--faint)">low rank</text>
        <text x={width - pad.right} y={height - 6} textAnchor="end" fontSize="10.5" fontFamily="var(--font-mono)" fill="var(--faint)">high rank</text>
      </svg>
    </div>
  );
}

function ChainLegend({ n }: { n: number }) {
  return (
    <div className="legend">
      {Array.from({ length: Math.max(1, n) }, (_, index) => (
        <span className="legend-item" key={index}>
          <span className="legend-swatch" style={{ background: CHAIN_COLORS[index % 4] }} />
          chain {index + 1}
        </span>
      ))}
    </div>
  );
}

function TabButton({ active, onClick, children }: { active: boolean; onClick: () => void; children: ReactNode }) {
  return (
    <button type="button" className={active ? "tab active" : "tab"} onClick={onClick}>
      {children}
    </button>
  );
}

function SelectInput({
  label,
  value,
  values,
  onChange
}: {
  label: string;
  value: string;
  values: readonly string[];
  onChange: (value: string) => void;
}) {
  return (
    <label>
      {label}
      <select value={value} onChange={(event) => onChange(event.target.value)}>
        {values.map((item) => <option key={item} value={item}>{item}</option>)}
      </select>
    </label>
  );
}

function TextInput({ label, value, onChange }: { label: string; value: string; onChange: (value: string) => void }) {
  return (
    <label>
      {label}
      <input value={value} onChange={(event) => onChange(event.target.value)} />
    </label>
  );
}

function NumberInput({
  label,
  value,
  min,
  step,
  onChange
}: {
  label: string;
  value: number;
  min?: number;
  step?: number;
  onChange: (value: number) => void;
}) {
  return (
    <label>
      {label}
      <input
        type="number"
        min={min}
        step={step}
        value={value}
        onChange={(event) => onChange(Number(event.target.value))}
      />
    </label>
  );
}

// One prior group: distribution selector + location/scale/df inputs. df is
// shown only for student_t (cauchy fixes df = 1; normal ignores it).
function PriorGroupControls({
  label,
  group,
  onChange
}: {
  label: string;
  group: PriorGroup;
  onChange: (patch: Partial<PriorGroup>) => void;
}) {
  return (
    <fieldset className="prior-group" style={{ border: "none", padding: 0, margin: "0.5rem 0 0" }}>
      <legend className="eyebrow" style={{ padding: 0 }}>{label}</legend>
      <div className="field-grid two">
        <SelectInput
          label="Distribution"
          value={group.distribution}
          values={PRIOR_DISTRIBUTIONS}
          onChange={(value) => onChange({ distribution: value as PriorGroup["distribution"] })}
        />
        <NumberInput label="Location (mean)" value={group.mean} step={0.5} onChange={(value) => onChange({ mean: value })} />
        <NumberInput label="Scale (sd)" value={group.sd} min={0} step={0.5} onChange={(value) => onChange({ sd: value })} />
        {group.distribution === "student_t" && (
          <NumberInput label="df" value={group.df} min={1} step={1} onChange={(value) => onChange({ df: value })} />
        )}
      </div>
    </fieldset>
  );
}

function parseMatrix(text: string): string[][] {
  const trimmed = text.replace(/\r\n?/g, "\n").replace(/\n+$/, "");
  if (trimmed.trim() === "") return [[""]];
  return trimmed.split("\n").map((line) => line.split(","));
}

function serializeMatrix(matrix: string[][]): string {
  return matrix.map((row) => row.join(",")).join("\n");
}

function CsvEditor({ label, value, onChange }: { label: string; value: string; onChange: (value: string) => void }) {
  const [view, setView] = useState<"grid" | "raw">("grid");
  const matrix = useMemo(() => parseMatrix(value), [value]);
  const headers = matrix[0] ?? [];
  const body = matrix.slice(1);

  function commit(next: string[][]) {
    onChange(serializeMatrix(next));
  }
  function setCell(rowIndex: number, colIndex: number, cell: string) {
    const next = matrix.map((row) => row.slice());
    next[rowIndex][colIndex] = cell;
    commit(next);
  }
  function addRow() {
    const next = matrix.map((row) => row.slice());
    next.push(Array(Math.max(headers.length, 1)).fill(""));
    commit(next);
  }
  function addColumn() {
    const next = matrix.map((row, index) => [...row, index === 0 ? `column_${headers.length + 1}` : ""]);
    commit(next);
  }
  function deleteRow(rowIndex: number) {
    if (matrix.length <= 2) return;
    commit(matrix.filter((_, index) => index !== rowIndex));
  }

  async function handleFile(event: ChangeEvent<HTMLInputElement>) {
    const file = event.target.files?.[0];
    event.target.value = "";
    if (!file) return;
    const name = file.name.toLowerCase();
    try {
      if (name.endsWith(".xlsx") || name.endsWith(".xls")) {
        const buffer = await file.arrayBuffer();
        const workbook = XLSX.read(buffer, { type: "array" });
        const sheet = workbook.Sheets[workbook.SheetNames[0]];
        onChange(XLSX.utils.sheet_to_csv(sheet).trim());
      } else {
        const text = (await file.text()).replace(/\r\n?/g, "\n").trim();
        const first = text.split("\n")[0] ?? "";
        const delimiter = !first.includes(",") && first.includes("\t")
          ? "\t"
          : !first.includes(",") && first.includes(";")
            ? ";"
            : null;
        onChange(delimiter ? text.split("\n").map((line) => line.split(delimiter).join(",")).join("\n") : text);
      }
      setView("grid");
    } catch (error) {
      console.error("Failed to read uploaded file", error);
    }
  }

  return (
    <div className="csv-editor">
      <div className="csv-toolbar">
        <span className="csv-title">{label}</span>
        <div className="csv-tools">
          <label className="seg upload" title="Upload CSV, TSV, TXT, or XLSX">
            Upload
            <input type="file" accept=".csv,.tsv,.txt,.xlsx,.xls" onChange={handleFile} />
          </label>
          <div className="csv-view-toggle" role="tablist" aria-label={`${label} view`}>
            <button type="button" className={view === "grid" ? "seg active" : "seg"} aria-pressed={view === "grid"} onClick={() => setView("grid")}>Grid</button>
            <button type="button" className={view === "raw" ? "seg active" : "seg"} aria-pressed={view === "raw"} onClick={() => setView("raw")}>Raw</button>
          </div>
        </div>
      </div>
      {view === "grid" ? (
        <>
          <div className="csv-grid-wrap">
            <table className="csv-grid">
              <thead>
                <tr>
                  <th className="rownum" scope="col"><span className="sr">row</span></th>
                  {headers.map((header, colIndex) => (
                    <th key={colIndex} scope="col">
                      <input value={header} spellCheck={false} aria-label={`column ${colIndex + 1} name`} onChange={(event) => setCell(0, colIndex, event.target.value)} />
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody>
                {body.map((row, bodyIndex) => (
                  <tr key={bodyIndex}>
                    <td className="rownum">
                      <span>{bodyIndex + 1}</span>
                      <button type="button" className="row-del" title="Delete row" aria-label={`delete row ${bodyIndex + 1}`} onClick={() => deleteRow(bodyIndex + 1)}>&times;</button>
                    </td>
                    {headers.map((_, colIndex) => (
                      <td key={colIndex}>
                        <input value={row[colIndex] ?? ""} spellCheck={false} aria-label={`row ${bodyIndex + 1} ${headers[colIndex] || colIndex + 1}`} onChange={(event) => setCell(bodyIndex + 1, colIndex, event.target.value)} />
                      </td>
                    ))}
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
          <div className="csv-actions">
            <button type="button" className="seg" onClick={addRow}>+ Row</button>
            <button type="button" className="seg" onClick={addColumn}>+ Column</button>
            <span className="csv-meta">{body.length} rows &middot; {headers.length} columns</span>
          </div>
        </>
      ) : (
        <textarea className="csv-raw" value={value} spellCheck={false} onChange={(event) => onChange(event.target.value)} />
      )}
    </div>
  );
}

/* ---------- distributions editor (grid <-> JSON source of truth) ---------- */

type DistType = "normal" | "bernoulli";

type DistRow = {
  cov: string;
  type: DistType;
  mean: string;
  sd: string;
  prob: string;
  correlation: string;
};

// Keep numbers numeric but pass through column-reference strings (e.g. "age_mean")
// so the grid round-trips both literal moments and AgD-column references.
function coerceCovariateValue(value: string): number | string {
  const trimmed = value.trim();
  if (trimmed === "") return "";
  const asNumber = Number(trimmed);
  return Number.isFinite(asNumber) && /^[-+]?[0-9.eE+]+$/.test(trimmed) ? asNumber : trimmed;
}

function correlationToText(correlation: Record<string, number> | undefined): string {
  if (!correlation) return "";
  return Object.entries(correlation)
    .map(([key, value]) => `${key}: ${value}`)
    .join(", ");
}

// Parse "age_std: 0.18, sex: -0.1" into { age_std: 0.18, sex: -0.1 }.
function parseCorrelationText(text: string): Record<string, number> | undefined {
  const entries = text
    .split(",")
    .map((part) => part.trim())
    .filter(Boolean)
    .map((part) => {
      const [key, raw] = part.split(":").map((piece) => piece.trim());
      const value = Number(raw);
      if (!key || !Number.isFinite(value)) return null;
      return [key, value] as const;
    })
    .filter((entry): entry is readonly [string, number] => entry !== null);
  if (entries.length === 0) return undefined;
  return Object.fromEntries(entries);
}

function specsToRows(json: string): { rows: DistRow[]; valid: boolean } {
  try {
    const parsed = JSON.parse(json) as Record<string, CovariateSpec & { correlation?: Record<string, number> }>;
    if (parsed === null || typeof parsed !== "object") return { rows: [], valid: false };
    const rows = Object.entries(parsed).map(([cov, spec]) => {
      const type: DistType = spec.type === "bernoulli" ? "bernoulli" : "normal";
      const normal = spec as Extract<CovariateSpec, { type: "normal" }>;
      const bernoulli = spec as Extract<CovariateSpec, { type: "bernoulli" }>;
      return {
        cov,
        type,
        mean: type === "normal" && normal.mean != null ? String(normal.mean) : "",
        sd: type === "normal" && normal.sd != null ? String(normal.sd) : "",
        prob: type === "bernoulli" && bernoulli.prob != null ? String(bernoulli.prob) : "",
        correlation: correlationToText(spec.correlation)
      } satisfies DistRow;
    });
    return { rows, valid: true };
  } catch {
    return { rows: [], valid: false };
  }
}

function rowsToSpecs(rows: DistRow[]): string {
  const specs: Record<string, CovariateSpec & { correlation?: Record<string, number> }> = {};
  for (const row of rows) {
    const name = row.cov.trim();
    if (name === "") continue;
    const correlation = parseCorrelationText(row.correlation);
    if (row.type === "bernoulli") {
      specs[name] = {
        type: "bernoulli",
        prob: coerceCovariateValue(row.prob),
        ...(correlation ? { correlation } : {})
      };
    } else {
      specs[name] = {
        type: "normal",
        mean: coerceCovariateValue(row.mean),
        sd: coerceCovariateValue(row.sd),
        ...(correlation ? { correlation } : {})
      };
    }
  }
  return JSON.stringify(specs, null, 2);
}

function DistributionsEditor({ value, onChange }: { value: string; onChange: (value: string) => void }) {
  const [view, setView] = useState<"grid" | "raw">("grid");
  const parsed = useMemo(() => specsToRows(value), [value]);
  const rows = parsed.rows;

  function commitRows(next: DistRow[]) {
    onChange(rowsToSpecs(next));
  }
  function setRow(index: number, patch: Partial<DistRow>) {
    commitRows(rows.map((row, rowIndex) => (rowIndex === index ? { ...row, ...patch } : row)));
  }
  function addRow() {
    commitRows([...rows, { cov: `covariate_${rows.length + 1}`, type: "normal", mean: "0", sd: "1", prob: "", correlation: "" }]);
  }
  function deleteRow(index: number) {
    commitRows(rows.filter((_, rowIndex) => rowIndex !== index));
  }

  return (
    <div className="csv-editor">
      <div className="csv-toolbar">
        <span className="csv-title">Comparator covariate distributions</span>
        <div className="csv-tools">
          <div className="seg-group csv-view-toggle" role="tablist" aria-label="Distributions view">
            <button type="button" className={view === "grid" ? "seg active" : "seg"} aria-pressed={view === "grid"} onClick={() => setView("grid")}>Grid</button>
            <button type="button" className={view === "raw" ? "seg active" : "seg"} aria-pressed={view === "raw"} onClick={() => setView("raw")}>Raw</button>
          </div>
        </div>
      </div>
      {view === "grid" ? (
        parsed.valid ? (
          <>
            <div className="csv-grid-wrap">
              <table className="csv-grid dist">
                <thead>
                  <tr>
                    <th className="rownum" scope="col"><span className="sr">row</span></th>
                    <th scope="col">Covariate</th>
                    <th scope="col">Type</th>
                    <th scope="col">Mean</th>
                    <th scope="col">SD</th>
                    <th scope="col">p</th>
                    <th scope="col">Correlation</th>
                  </tr>
                </thead>
                <tbody>
                  {rows.map((row, index) => (
                    <tr key={index}>
                      <td className="rownum">
                        <span>{index + 1}</span>
                        <button type="button" className="row-del" title="Delete covariate" aria-label={`delete covariate ${index + 1}`} onClick={() => deleteRow(index)}>&times;</button>
                      </td>
                      <td>
                        <input value={row.cov} spellCheck={false} aria-label={`covariate ${index + 1} name`} onChange={(event) => setRow(index, { cov: event.target.value })} />
                      </td>
                      <td className="sel-cell">
                        <select value={row.type} aria-label={`covariate ${index + 1} type`} onChange={(event) => setRow(index, { type: event.target.value as DistType })}>
                          <option value="normal">normal</option>
                          <option value="bernoulli">bernoulli</option>
                        </select>
                      </td>
                      <td>
                        <input value={row.mean} placeholder={row.type === "normal" ? "0" : ""} disabled={row.type !== "normal"} spellCheck={false} aria-label={`covariate ${index + 1} mean`} onChange={(event) => setRow(index, { mean: event.target.value })} />
                      </td>
                      <td>
                        <input value={row.sd} placeholder={row.type === "normal" ? "1" : ""} disabled={row.type !== "normal"} spellCheck={false} aria-label={`covariate ${index + 1} sd`} onChange={(event) => setRow(index, { sd: event.target.value })} />
                      </td>
                      <td>
                        <input value={row.prob} placeholder={row.type === "bernoulli" ? "0.5" : ""} disabled={row.type !== "bernoulli"} spellCheck={false} aria-label={`covariate ${index + 1} probability`} onChange={(event) => setRow(index, { prob: event.target.value })} />
                      </td>
                      <td>
                        <input value={row.correlation} placeholder="none" spellCheck={false} aria-label={`covariate ${index + 1} correlation`} onChange={(event) => setRow(index, { correlation: event.target.value })} />
                      </td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <div className="csv-actions">
              <button type="button" className="seg" onClick={addRow}>+ Covariate</button>
              <span className="csv-meta">{rows.length} {rows.length === 1 ? "covariate" : "covariates"} &middot; Gaussian copula correlation</span>
            </div>
          </>
        ) : (
          <div className="error-box">The distributions JSON is not valid, so the grid is unavailable. Fix it in the Raw view.</div>
        )
      ) : (
        <textarea className="csv-raw" value={value} spellCheck={false} onChange={(event) => onChange(event.target.value)} />
      )}
    </div>
  );
}

/* ---------- Stan JSON editor (read-only: generated field/value/description) ---------- */

type StanField = { field: string; value: string; description: string };

const STAN_FIELD_DESCRIPTIONS: Record<string, string> = {
  ns_ipd: "IPD studies",
  ni_ipd: "IPD individuals",
  ns_agd: "AgD studies",
  ni_agd: "AgD data points",
  nt: "Treatments",
  nX: "Covariates",
  narm_agd: "AgD arms",
  ipd_trt: "IPD treatment index",
  ipd_y: "IPD outcomes",
  ipd_X: "IPD covariate matrix",
  agd_r: "AgD events",
  agd_n: "AgD totals",
  agd_E: "AgD exposure",
  agd_y: "AgD means",
  agd_se: "AgD standard errors",
  agd_int_X: "Integration covariate points",
  nint: "Integration points",
  link: "Link function code",
  dist: "Survival distribution code",
  n_aux: "Auxiliary parameters",
  prior_intercept_sd: "Prior SD, intercept",
  prior_trt_sd: "Prior SD, treatment",
  prior_reg_sd: "Prior SD, regression"
};

// Render any generated value compactly: scalars as-is, arrays elided after a few
// entries so the table stays scannable (the Raw view shows the full JSON).
function previewStanValue(value: unknown): string {
  if (Array.isArray(value)) {
    const head = value.slice(0, 3).map((item) => previewStanValue(item));
    const suffix = value.length > 3 ? ", …" : "";
    return `[${head.join(", ")}${suffix}]`;
  }
  if (typeof value === "number") {
    return Number.isInteger(value) ? String(value) : String(Number(value.toFixed(4)));
  }
  return String(value);
}

function stanJsonToFields(json: string): { fields: StanField[]; valid: boolean } {
  if (json.trim() === "") return { fields: [], valid: true };
  try {
    const parsed = JSON.parse(json) as Record<string, unknown>;
    if (parsed === null || typeof parsed !== "object") return { fields: [], valid: false };
    const fields = Object.entries(parsed).map(([field, value]) => ({
      field,
      value: previewStanValue(value),
      description: STAN_FIELD_DESCRIPTIONS[field] ?? ""
    }));
    return { fields, valid: true };
  } catch {
    return { fields: [], valid: false };
  }
}

function StanJsonEditor({ stanJson }: { stanJson: string }) {
  const [view, setView] = useState<"grid" | "raw">("grid");
  const parsed = useMemo(() => stanJsonToFields(stanJson), [stanJson]);

  return (
    <div className="csv-editor">
      <div className="csv-toolbar">
        <span className="csv-title">Stan data JSON</span>
        <div className="csv-tools">
          <span className="ro-note">read-only &middot; generated</span>
          <div className="seg-group csv-view-toggle" role="tablist" aria-label="Stan JSON view">
            <button type="button" className={view === "grid" ? "seg active" : "seg"} aria-pressed={view === "grid"} onClick={() => setView("grid")}>Grid</button>
            <button type="button" className={view === "raw" ? "seg active" : "seg"} aria-pressed={view === "raw"} onClick={() => setView("raw")}>Raw</button>
          </div>
        </div>
      </div>
      {view === "grid" ? (
        parsed.fields.length === 0 ? (
          <EmptyState title="No Stan data yet" body="Run Prepare Stan Data or Run Model to generate the Stan data JSON from your inputs." />
        ) : (
          <>
            <div className="csv-grid-wrap">
              <table className="csv-grid kv">
                <thead>
                  <tr>
                    <th className="rownum" scope="col"><span className="sr">row</span></th>
                    <th scope="col">Field</th>
                    <th scope="col">Value</th>
                    <th scope="col">Description</th>
                  </tr>
                </thead>
                <tbody>
                  {parsed.fields.map((row, index) => (
                    <tr key={row.field}>
                      <td className="rownum">{index + 1}</td>
                      <td className="static key">{row.field}</td>
                      <td className="static">{row.value}</td>
                      <td className="static desc">{row.description}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <div className="csv-actions">
              <span className="csv-meta">{parsed.fields.length} fields &middot; generated from your inputs</span>
            </div>
          </>
        )
      ) : (
        <textarea className="csv-raw" value={stanJson} readOnly spellCheck={false} />
      )}
    </div>
  );
}

function splitList(value: string): string[] {
  return value.split(",").map((item) => item.trim()).filter(Boolean);
}

function selectedModelTypes(model: ModelRunMode): ModelType[] {
  return model === "both" ? ["spfa", "relaxed"] : [model];
}

function resolveModelName(family: Family, distribution: SurvivalDistribution, model: ModelType): string {
  return family === "survival" ? survivalModelName(distribution, model) : modelNameFor(family, model);
}

function buildComparisonRows(
  runs: Partial<Record<ModelType, StanRun>>,
  family: Family,
  link: LinkName
): ComparisonRow[] {
  if (!runs.spfa || !runs.relaxed) return [];
  const spfaRows = familyAwareReportSummaryRows(runs.spfa, family, link);
  const relaxedRows = familyAwareReportSummaryRows(runs.relaxed, family, link);
  const primary = [
    ...importantPlotVariables(spfaRows, family),
    ...importantPlotVariables(relaxedRows, family)
  ];
  const names = Array.from(new Set(primary.length > 0 ? primary : [
    ...spfaRows.filter((row) => row.group === "Treatment effects").map((row) => row.variable),
    ...relaxedRows.filter((row) => row.group === "Treatment effects").map((row) => row.variable)
  ]));
  return names.map((variable) => ({
    variable,
    spfa: spfaRows.find((row) => row.variable === variable),
    relaxed: relaxedRows.find((row) => row.variable === variable)
  }));
}

function groupedRows(rows: DrawSummaryRow[]): Array<[string, DrawSummaryRow[]]> {
  const grouped = new Map<string, DrawSummaryRow[]>();
  for (const row of rows) grouped.set(row.group, [...(grouped.get(row.group) ?? []), row]);
  return Array.from(grouped.entries());
}

function extent(values: number[]): [number, number] {
  const finite = values.filter(Number.isFinite);
  if (finite.length === 0) return [0, 1];
  const min = Math.min(...finite);
  const max = Math.max(...finite);
  if (min === max) return [min - 0.5, max + 0.5];
  return [min, max];
}

function mean(values: number[]): number {
  const finite = values.filter(Number.isFinite);
  if (finite.length === 0) return NaN;
  return finite.reduce((sum, value) => sum + value, 0) / finite.length;
}

function niceTicks(min: number, max: number, count: number): number[] {
  const span = max - min || 1;
  const step0 = span / count;
  const mag = Math.pow(10, Math.floor(Math.log10(step0)));
  const norm = step0 / mag;
  const step = (norm >= 5 ? 5 : norm >= 2 ? 2 : 1) * mag;
  const start = Math.ceil(min / step) * step;
  const ticks: number[] = [];
  for (let tick = start; tick <= max + step * 0.001; tick += step) ticks.push(Number(tick.toFixed(10)));
  return ticks;
}

function makeHistogram(values: number[], nBins: number): Array<{ start: number; end: number; count: number }> {
  const [min, max] = extent(values);
  const width = (max - min) / nBins;
  const bins = Array.from({ length: nBins }, (_, index) => ({
    start: min + index * width,
    end: min + (index + 1) * width,
    count: 0
  }));
  for (const value of values) {
    const index = Math.min(nBins - 1, Math.max(0, Math.floor((value - min) / Math.max(width, 1e-12))));
    bins[index].count += 1;
  }
  return bins;
}

function rankBins(chains: number[][], nBins: number): number[][] {
  const ranked = chains
    .flatMap((chain, chainIndex) => chain.map((value) => ({ value, chainIndex })))
    .sort((a, b) => a.value - b.value);
  const bins = Array.from({ length: chains.length }, () => Array(nBins).fill(0));
  const denom = Math.max(1, ranked.length - 1);
  ranked.forEach((item, rank) => {
    const bin = Math.min(nBins - 1, Math.floor((rank / denom) * nBins));
    bins[item.chainIndex][bin] += 1;
  });
  return bins;
}

// Compact chart number formatter (3 sig figs; exponential for very small/large).
function fmt(value: number | null | undefined, sig?: number): string {
  if (value === null || value === undefined || Number.isNaN(value)) return "n/a";
  const abs = Math.abs(value);
  if (abs !== 0 && (abs < 0.001 || abs >= 1e5)) return value.toExponential(2);
  const digits = sig ?? (abs >= 100 ? 1 : abs >= 10 ? 2 : 3);
  return value.toFixed(digits).replace(/\.?0+$/, (m) => (m.includes(".") ? "" : m));
}

function shortLabel(value: string, maxLength: number): string {
  return value.length <= maxLength ? value : `${value.slice(0, maxLength - 1)}…`;
}

function downloadJson(filename: string, value: unknown) {
  const blob = new Blob([JSON.stringify(value, null, 2)], { type: "application/json" });
  const url = URL.createObjectURL(blob);
  const anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = filename;
  anchor.click();
  URL.revokeObjectURL(url);
}
