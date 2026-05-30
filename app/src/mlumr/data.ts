import type {
  ColumnMapping,
  Family,
  MlumrData,
  RawRow,
  StandardizedAgdRow,
  StandardizedIpdRow
} from "./types";

export function buildMlumrData(
  family: Family,
  ipdRows: RawRow[],
  agdRows: RawRow[],
  mapping: ColumnMapping
): MlumrData {
  if (ipdRows.length === 0) throw new Error("IPD data must contain at least one row.");
  if (agdRows.length === 0) throw new Error("AgD data must contain at least one row.");
  if (mapping.covariates.length === 0) throw new Error("At least one covariate is required.");

  const ipd = ipdRows.map((row) => standardizeIpd(row, family, mapping));
  const agd = agdRows.map((row) => standardizeAgd(row, family, mapping));
  const indexTreatment = uniqueSingle(ipd.map((row) => row.treatment), "IPD treatment");
  const comparatorTreatment = uniqueSingle(agd.map((row) => row.treatment), "AgD treatment");

  return {
    family,
    covariates: mapping.covariates,
    ipd,
    agd,
    indexTreatment,
    comparatorTreatment
  };
}

function standardizeIpd(row: RawRow, family: Family, mapping: ColumnMapping): StandardizedIpdRow {
  if (family === "survival") {
    return standardizeSurvivalIpd(row, mapping);
  }

  const outcome = numeric(row, mapping.outcome);
  if (family === "binomial" && !(outcome === 0 || outcome === 1)) {
    throw new Error("Binomial IPD outcomes must be 0/1.");
  }
  if (family === "poisson" && (!Number.isInteger(outcome) || outcome < 0)) {
    throw new Error("Poisson IPD outcomes must be non-negative integer counts.");
  }

  const out: StandardizedIpdRow = {
    study: "IPD_Study",
    treatment: stringValue(row, mapping.treatment),
    outcome,
    covariates: mapping.covariates.map((covariate) => numeric(row, covariate))
  };

  if (family === "poisson") {
    if (!mapping.exposure) throw new Error("Poisson IPD requires an exposure column.");
    out.exposure = positiveNumeric(row, mapping.exposure);
  }

  return out;
}

function standardizeSurvivalIpd(row: RawRow, mapping: ColumnMapping): StandardizedIpdRow {
  if (!mapping.time) throw new Error("Survival IPD requires a time column.");
  if (!mapping.status) throw new Error("Survival IPD requires a status column.");

  const time = positiveNumeric(row, mapping.time);
  const statusRaw = Number(row[mapping.status]);
  if (!(statusRaw === 0 || statusRaw === 1)) {
    throw new Error("Survival IPD status must be 0 (censored) or 1 (event).");
  }
  const delayTime = mapping.entryTime ? nonNegativeNumeric(row, mapping.entryTime) : 0;

  return {
    study: "IPD_Study",
    treatment: stringValue(row, mapping.treatment),
    outcome: 0, // Not used for survival; placeholder.
    covariates: mapping.covariates.map((covariate) => numeric(row, covariate)),
    time,
    status: statusRaw,
    startTime: 0,
    delayTime
  };
}

function standardizeAgd(row: RawRow, family: Family, mapping: ColumnMapping): StandardizedAgdRow {
  if (family === "survival") {
    return standardizeSurvivalAgd(row, mapping);
  }

  const out: StandardizedAgdRow = {
    treatment: stringValue(row, mapping.treatment),
    source: row
  };

  if (family === "binomial") {
    if (!mapping.agdEvents || !mapping.agdTotal) {
      throw new Error("Binomial AgD requires event and total columns.");
    }
    out.r = integerCount(row, mapping.agdEvents);
    out.n = integerCount(row, mapping.agdTotal, 1);
    if (out.r > out.n) throw new Error("Binomial AgD events cannot exceed total.");
  } else if (family === "normal") {
    if (!mapping.agdMean || !mapping.agdSe) {
      throw new Error("Normal AgD requires mean and standard-error columns.");
    }
    out.y = numeric(row, mapping.agdMean);
    out.se = positiveNumeric(row, mapping.agdSe);
  } else {
    if (!mapping.agdEvents || !mapping.agdExposure) {
      throw new Error("Poisson AgD requires event and exposure columns.");
    }
    out.r = integerCount(row, mapping.agdEvents);
    out.exposure = positiveNumeric(row, mapping.agdExposure);
  }

  return out;
}

function standardizeSurvivalAgd(row: RawRow, mapping: ColumnMapping): StandardizedAgdRow {
  if (!mapping.agdTime) throw new Error("Survival AgD requires an agdTime column.");
  if (!mapping.agdStatus) throw new Error("Survival AgD requires an agdStatus column.");

  const time = positiveNumeric(row, mapping.agdTime);
  const statusRaw = Number(row[mapping.agdStatus]);
  if (!(statusRaw === 0 || statusRaw === 1)) {
    throw new Error("Survival AgD status must be 0 (censored) or 1 (event).");
  }
  const delayTime = mapping.agdEntryTime ? nonNegativeNumeric(row, mapping.agdEntryTime) : 0;

  return {
    treatment: stringValue(row, mapping.treatment),
    source: row,
    time,
    status: statusRaw,
    startTime: 0,
    delayTime
  };
}

export function numeric(row: RawRow, column: string): number {
  if (!(column in row)) throw new Error(`Missing column '${column}'.`);
  const value = Number(row[column]);
  if (!Number.isFinite(value)) throw new Error(`Column '${column}' must contain finite numeric values.`);
  return value;
}

export function positiveNumeric(row: RawRow, column: string): number {
  const value = numeric(row, column);
  if (value <= 0) throw new Error(`Column '${column}' must contain positive numeric values.`);
  return value;
}

export function nonNegativeNumeric(row: RawRow, column: string): number {
  const value = numeric(row, column);
  if (value < 0) throw new Error(`Column '${column}' must contain non-negative numeric values.`);
  return value;
}

export function integerCount(row: RawRow, column: string, lower = 0): number {
  const value = numeric(row, column);
  if (!Number.isInteger(value) || value < lower) {
    throw new Error(`Column '${column}' must contain integer values >= ${lower}.`);
  }
  return value;
}

function stringValue(row: RawRow, column: string): string {
  if (!(column in row)) throw new Error(`Missing column '${column}'.`);
  const value = row[column];
  if (value === null || value === "") throw new Error(`Column '${column}' must not be empty.`);
  return String(value);
}

function uniqueSingle(values: string[], label: string): string {
  const unique = Array.from(new Set(values));
  if (unique.length !== 1) {
    throw new Error(`${label} must contain exactly one value. Found: ${unique.join(", ")}.`);
  }
  return unique[0];
}
