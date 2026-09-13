@chapter(Two trials, no common arm)
@cue(scene = "evidence")
@cue(pIndex = 0.2)
@cue(pComparator = 0.8)
@cue(target = 0.5)
@board(question: "Compare A and B in which population?")

Imagine two treatments, A and B, that were never tested in the same trial. For treatment A, you have the data for every patient. For treatment B, you only have what its trial published: counts and averages. No shared control group links the two trials. This situation is called an unanchored comparison.

This lesson teaches one way to make that comparison. It is called multilevel unanchored meta-regression, or M L U M R. We will build the ideas one small step at a time, and then use the M L U M R package for R. The lesson follows the development version of the package, which is heading toward version zero point two.

The blue trial, trial A, recorded each patient's outcome and characteristics. The orange trial, trial B, reported only totals and summaries. Our outcome is a harmful event, so a lower risk is better. Every number you see is made up for teaching. None of it is a real clinical result.

Some patients carry a marker that raises their risk. The two trials enrolled very different shares of patients with the marker. Look first at the crude difference. It compares each treatment inside its own trial. Now make the two trials more alike.
@cue(pComparator -> 0.2, over: 3s)
The crude difference changed, yet neither treatment changed. Only the patients changed. The target difference did not move, because it compares both treatments in one chosen population, called the target population. Keeping those two questions apart is where population adjustment begins.
@pause(prompt: "Move the two trial sliders, then the target slider. Which difference reacts to each one?")

@chapter(What adjustment has to assume)
@cue(scene = "assumptions")
@cue(shift = 0)
@cue(target = 0.5)
@clear(board)
@board(assumption: "In this design, the treatment and the trial always go together.")

Removing the need for a shared control group does not remove the need for assumptions. For an unanchored comparison to work, patients with the same characteristics must be expected to have the same outcome on a given treatment, whichever trial enrolled them. That means measuring every factor that affects the outcome, including factors that change how well a treatment works. The patients in the two trials must also overlap, and the treatments, outcomes and follow-up must mean the same thing in both.

Suppose trial B also differed in something nobody measured, such as the quality of care. We can add that as a hidden shift in trial B's risk.
@cue(shift -> 0.8, over: 3s)
Trial B's reported risk goes up. But trial B has only one starting level for its risk, so it cannot tell us how much of that rise came from the treatment and how much came from the trial. Adjusting for the marker does not help, because the marker was not the problem.

The same thing happens with a missing risk factor, a different way of measuring the outcome, or a model that predicts badly outside the data. More patients can make a biased answer look more precise. More careful computation cannot fix it either. Reading the result as a causal effect needs assumptions that the data cannot check.
@pause(prompt: "Slide the hidden difference through zero. Can the reported risk alone tell you how much came from the treatment?")

@chapter(Shared or separate slopes)
@cue(scene = "response")
@cue(betaB = 2.4)
@cue(target = 0.5)
@clear(board)
@board(model: $g(\theta_k(x))=\alpha_k+x^T\beta_k$)
@board(shared: "Shared slopes: both treatments get the same covariate effects, on the link scale.")

The outcome model gives each treatment its own starting level, called an intercept. Slopes, also called coefficients, describe how much a patient characteristic, called a covariate, raises or lowers the risk. A covariate that affects the outcome, like our marker, is called a prognostic factor. The shared prognostic factor assumption, or S P F A, says that both treatments have the same slopes on the scale where the model is a straight-line, called the link scale. Here that scale is the log odds.

Each line on the chart shows one treatment's log odds, for patients without the marker and with it. Both slopes are two point four, so the lines are parallel. The gap between them is the same for every patient. That means the odds ratio for any single patient does not depend on the marker.

Now give trial B a different slope.
@cue(betaB -> 0.8, over: 3s)
The lines are no longer parallel. The gap now depends on the marker, so the marker changes how well treatment A works compared with treatment B. Statisticians call this effect modification. This is the relaxed model, where each treatment has its own slopes.

That flexibility comes at a price. Trial A's patient data can estimate trial A's slopes. Trial B's slopes can only come from its published summaries, mainly the differences between its subgroups, and from the prior, which is what you assume about a value before seeing the data. Writing down a relaxed model does not mean the data can actually estimate it.
@pause(prompt: "Set trial B's slope back to two point four, then lower it. Which odds ratio stays the same, and which one changes?")

@chapter(Average the predictions)
@cue(scene = "integration")
@cue(width = 0)
@cue(points = 16)
@clear(board)
@board(integral: $\bar\theta_{ks}=\int g^{-1}(\alpha_k+x^T\beta_k)f_s(x)\,dx$)

Trial B only reports averages, like the share of its patients who had the event. To connect those averages to a model for individual patients, the model has to average in the same way. It predicts the risk for each kind of patient, and then averages those risks over trial B's population. This averaging step is called integration.

At first, every patient has the same covariate value, zero. Then the risk of the average patient and the average risk are the same thing. Now spread the patients out, while keeping their average exactly at zero.
@cue(width -> 1.5, over: 4s)
The curve bends. Patients above the average gain more risk than patients below it lose. So the average risk rises, while the risk of the average patient stays where it was. Replacing a whole population with one average patient gives a different answer.

The dots are the points used to do the averaging. Here they are simply spaced evenly, so you can see them. The package instead uses Sobol points, an evenly spread sequence drawn from the covariate distributions you choose. These points are a calculation tool. They are not real patients.
@cue(points = 4)
@cue(points -> 128, over: 4s)
With more points, the average becomes more accurate. In a real analysis, check the integration, and check that the effect you report stops changing when you add points. The package's integration check compares covariate means and standard deviations on two grids, and stable values there do not prove that the effect is stable. Below the chart, you can run this same calculation in R, right in your browser.
@pause(prompt: "Spread the patients out, then compare four points with one hundred twenty-eight. Which change moves the average risk itself, and which only makes the calculation more accurate?")

@chapter(Rebuild the population)
@cue(scene = "dependence")
@cue(rho = 0)
@clear(board)
@board(joint: "Summaries for each covariate, plus how they go together, define the population to average over.")

A table of covariate means and standard deviations does not fully describe a population. Picture two markers, each present in half of the patients. If the markers are unrelated, the four kinds of patient are equally common: neither marker, only the first, only the second, or both.

Now make the two markers tend to appear together.
@cue(rho -> 1, over: 3s)
More patients now have both markers, or neither. Each marker is still present in exactly half of the patients. Yet the average risk changes, because the risk curve bends: having both markers adds more risk than the two markers add one at a time.
@cue(rho -> -1, over: 4s)
With a negative correlation, patients tend to have exactly one of the two markers. The same published summaries can hide different average risks.

By default, the package estimates how the covariates go together from trial A's patient data, and uses that for trial B. Carrying it over is an assumption, so look at the result and try other plausible values. One more detail: when the shares are not both one half, some correlations are impossible. Here every value works, because both shares are exactly one half.
@pause(prompt: "Keep each marker at half the patients and change only the correlation. Why does the average risk move?")

@chapter(Choose what to estimate)
@cue(scene = "target")
@cue(betaB = 2.4)
@cue(target = 0.1)
@clear(board)
@board(target: $\Delta_{RD}(P)=E_P[\theta_A(X)]-E_P[\theta_B(X)]$)
@board(order: "In each posterior draw: predict, average, then compare.")

An estimand is a precise statement of what you want to estimate. It names the two treatments, the outcome, the population and the effect scale. For survival, it also names a time. The population could be trial A's patients, trial B's patients, or another group you care about. Choose it from the decision you need to make, not from a software default.

Both treatments now share their slopes. Move the target population from mostly without the marker to mostly with it.
@cue(target -> 0.9, over: 5s)
Every patient has the same odds ratio, but the population odds ratio moves. This surprising behavior is called non-collapsibility. It happens even without effect modification, and even without confounding. The risk difference also changes, because it depends on the baseline risks and on the mix of patients.

The package fits a Bayesian model, which produces thousands of plausible sets of parameter values, called posterior draws. Inside every draw it predicts, averages and compares, and only then summarizes. For trial B's own population, subgroups are weighted by their number of patients for binary and continuous outcomes, and by exposure time for counts. These population weights are not the same thing as how precise each subgroup is. If you supply your own target rows, each row counts equally. And to report an odds ratio, exponentiate each log odds ratio draw first, then summarize. Exponentiating the average log odds ratio gives a different number.

You can ask for effects in trial A's population, in trial B's population, or in your own target population. A target changes the question, not the data. It adds no outcomes, and it does not refit the model. Effects for one particular kind of patient are a separate request, called conditional effects.
@pause(prompt: "Compare target shares of zero, one half and one. Why do the patient and population odds ratios agree at zero and at one?")

@chapter(What subgroup rows can tell you)
@cue(scene = "identification")
@cue(design = "one")
@cue(separation = 0.8)
@cue(targetX = 1)
@cue(priorSD = 2)
@clear(board)
@board(rank: "K slopes plus one intercept need information in K + 1 directions.")

Here is a question about the relaxed model: can trial B's summaries pin down trial B's own slope? To see this clearly, we switch to a continuous outcome with a straight-line model. Each subgroup row, one line of trial B's published table, reports its mean outcome and how precise that mean is. A row whose patients average zero on the covariate tells us the height of the line at zero. It cannot tell us both the height and the slope.

Add a second, separate row at the same covariate value.
@cue(design = "duplicate")
There are now two rows, but they still point in only one direction. They make the estimate at zero more precise. They do not reveal the slope. These are two different groups of patients, not the same row counted twice.

Now give the two rows different covariate values.
@cue(design = "separated")
Now the data can separate the intercept from the slope. Move the two rows close together.
@cue(separation -> 0.02, over: 4s)
Technically, the slope is still identified, meaning the data can pin it down in principle. But predicting a target far from both rows becomes very uncertain. Being identified and being precise are different things.

For a straight-line model, this reasoning is exact. With a curved link, as for binary or count outcomes, or a log link, the spread of patients inside each row matters too. So when there are enough rows, the package's subgroup check only describes them, and reports its flag as missing rather than as a pass. It does not accept survival data at all.
@pause(prompt: "Compare one row, two rows in the same place, and two separated rows. Then move the target to zero.")

@chapter(Run mlumr in your browser)
@cue(scene = "workflow")
@cue(step = 0)
@clear(board)
@board(workflow: "Define → prepare → integrate → fit → check → report")

Now let's turn these ideas into a real analysis with the package. The chart shows six steps. Further down this page, a code cell runs real package code, and the real Stan model, inside your browser. The first step needs no code at all. Write down the two treatments, the outcome, the target population and the effect scale before you touch the data.
@cue(step = 1)
Next, prepare the data. One function describes trial A's patient data, and the package calls treatment A the index treatment. Another describes trial B's published summaries, and calls treatment B the comparator. A third function combines them. Column names are written in quotes. If trial B reports subgroups, each patient must belong to exactly one of them. Tables that overlap, such as one by age and another by sex, cannot be stacked as if they were separate groups.
@cue(step = 2)
Then describe trial B's covariates with distributions, and add integration points. Choose a distribution that fits each covariate's range. A covariate that is itself a proportion, such as the share of skin affected, should not get a normal distribution that allows values below zero. Check the integration, and if you plan a relaxed model, check what the subgroups can identify.
@cue(step = 3)
Now fit the model, with the priors written out, a seed, and enough sampling. The default engine is R Stan, and command Stan R also works. In the relaxed model, trial B's slopes can have their own prior, which makes sensitivity analysis easy. Choose prior scales that make sense for each covariate's units.
@cue(step = 4)
Before trusting any number, check the fit. The fitting algorithm, called the sampler, runs several independent chains of draws. The package checks those chains when it finishes and warns about problems, and the summary shows the checks again. Centering the covariates, which the package does by default, and the optional Q R setting can both help the sampler. With centering, the intercept describes a patient with average covariates, so your intercept prior applies to that patient, not to one whose covariates are all zero. With Q R, your priors still apply to the original coefficients. Neither setting creates information, or makes the trials comparable.
@cue(step = 5)
Finally, ask for the population and the effect you planned. The effect called L O R is a log odds ratio. The package also offers two quick benchmarks. The naive comparison simply compares the two trials as they are. S T C, the simulated treatment comparison, fits a regression to trial A, and predicts what treatment A would do in trial B's population. It then compares that prediction with trial B's reported outcome. That question does not need equal slopes, but carrying its answer to another population needs more justification.
@pause(prompt: "Step through the six code panels. Then run the cell below: first the R code, then the Stan fit.")

@chapter(Pick the outcome model)
@cue(scene = "families")
@cue(family = 0)
@clear(board)
@board(family: "Match the likelihood, the link, the summaries and the effect scale.")

The package handles four kinds of outcome: binary, continuous, counts and survival. Most function names are shared, but survival uses its own function for trial B's data, and each outcome needs different data and reports different effects.

A binary outcome is yes or no. Trial A gives each patient's result, and trial B gives its number of events and number of patients. The logit link is the default, with probit and complementary log-log as alternatives. Risk differences, risk ratios and log odds ratios are all built from risks averaged over the population.
@cue(family = 1)
A continuous outcome uses a normal model. Trial B must report its mean outcome and the standard error of that mean, not the standard deviation of individual patients. The covariate standard deviations are a different thing: they describe how the covariates are spread. Even with a log link, give trial B's mean and its standard error on the original scale, not the log scale. If a paper reports a standard deviation instead, divide it by the square root of the number of patients.
@cue(family = 2)
Counts need exposure, such as years of follow-up. The effect is a rate ratio, where one means no difference. Keep the exposure units the same in both trials. And if follow-up time depends on the covariates, trial B's covariate summaries should weight each patient by follow-up time, which ordinary published averages usually do not.
@cue(family = 3)
Survival outcomes use event times, plus censored times for patients whose follow-up ended before an event. For trial B, you supply event times reconstructed from its published survival curve, together with covariate summaries. Reconstruction does not recover trial B's individual covariates, and its own uncertainty is not carried into the fit. The package offers several parametric and flexible baseline shapes, each with its own rules. Open the list of survival distributions in the panel to see them.
@pause(prompt: "Pick an outcome type. What must trial B report, and which value of the effect means no difference?")

@chapter(Survival after averaging)
@cue(scene = "survival")
@cue(time = 0)
@cue(target = 0.5)
@cue(heterogeneity = 1.8)
@clear(board)
@board(survival: $\bar S_k(t)=E[S_k(t\mid X)]$)
@board(hazard: $\bar h_k(t)=\frac{E[h_k(t\mid X)S_k(t\mid X)]}{E[S_k(t\mid X)]}$)

This survival example has two risk groups: patients with the marker, whose hazard is higher, and patients without it. Each patient's survival follows a simple exponential curve, and for every patient, the hazard ratio of A versus B is zero point six five. The target population starts as an equal mix of the two groups.

Now move forward through follow-up.
@cue(time -> 24, over: 7s)
High-risk patients have their events sooner, so they leave the group still at risk. The mix of survivors changes, and it changes faster under treatment B. The population hazard is an average over the survivors, not over everyone who started.

That is why the population hazard ratio changes over time, even though every patient's hazard ratio stays constant. Now remove the difference between the risk groups.
@cue(heterogeneity -> 0, over: 3s)
With identical risk groups, the survivors never change their mix, and the population hazard ratio stays at zero point six five.
@cue(heterogeneity -> 1.8, over: 3s)

Restricted mean survival time, or R M S T, is the area under a survival curve up to a chosen time, called the horizon. In plain terms, it is the average time patients stay event free up to that horizon. The shaded area between the two curves is the R M S T difference, measured in months. Change the horizon, and you change the question. Only compare R M S T results that use the same horizon and the same time units.

In the package, each trial gets its own baseline shape by default, whenever the distribution has a shape. Sharing one shape is a stronger assumption, but separate shapes also assume that each shape travels with its treatment, so fit both and compare. A population hazard ratio always needs a time. For accelerated failure time models, the population number is a time ratio only when the slopes and the shape are both shared. Otherwise, the package labels it differently, so it is not mistaken for a time ratio. Patients still event free when follow-up ends, events known only to fall within a time window, and patients who join follow-up late each need their own likelihood terms. A simple count of events cannot replace them.
@pause(prompt: "Compare zero, twelve and twenty-four months. Then remove the risk difference between groups. What happens to the population hazard ratio, and why?")

@chapter(When the prior matters)
@cue(scene = "priors")
@cue(design = "one")
@cue(targetX = 1)
@cue(separation = 0.8)
@cue(priorSD = 3)
@clear(board)
@board(prior: "A prior can narrow an answer. It never adds an observed subgroup.")

Back to the straight-line example. With one row at zero, a target at one depends on a slope the data cannot see. The band shows the answer using normal priors centered at zero, on both the intercept and the slope.

Make the prior tighter.
@cue(priorSD -> 0.3, over: 4s)
The interval at the target shrinks a lot. No new patient was observed. The answer is narrower only because the assumption became stronger.

Now move the target to where the row is.
@cue(targetX -> 0, over: 3s)
Here the data alone pin down the target, even though the slope is still unknown. So a verdict about the coefficients is not automatically a verdict about your target. For a continuous outcome with a straight-line model, the package's identification check asks this question for trial A's population, once with the integration points the model uses, and once with the means trial B published.

Check prior sensitivity for the target you will actually report. Results in trial B's population can look stable, while results in trial A's population, or in a new target, still depend on the prior. Try several reasonable priors, see how far the result moves under each, and report what you find. Never pick the prior that happens to give the narrowest interval.
@pause(prompt: "Change the prior with the target at zero, then at one. Repeat with two separated rows. Which narrowing comes from data, and which from assumptions?")

@chapter(Read a fit before trusting it)
@cue(scene = "diagnostics")
@cue(diagnostic = 0)
@clear(board)
@board(checks: "Good sampling, identification and comparable trials are three separate questions.")

Suppose a model hands you a neat table of results. Before reading it, look at the sampling checks. Did every chain run? Were there divergent transitions, where the sampler could not explore part of the posterior? Then check R hat, the effective sample size, which says how many independent draws the chains are worth, and tree depth. A missing or infinite check is not good news. Raising a sampler setting can help, but it does not fix a badly shaped posterior by itself.
@cue(diagnostic = 1)
R hat compares the chains with each other, and values close to one mean they agree. When chains settle in different places, R hat grows. A value of infinity usually means the chains got stuck, each at its own value.
@cue(diagnostic = 2)
Next, keep what the data can tell you apart from how well the computer ran. When the subgroup check reports a missing flag for a binary outcome, it is holding back a verdict on purpose. With a curved link, subgroup means alone cannot show what the rows identify, because the spread inside each row matters too.
@cue(diagnostic = 3)
Check the numerical integration on its own. More points reduce approximation error. But neither a stable grid nor good sampling shows that the covariate distributions you chose are right.
@cue(diagnostic = 4)
Look at how the results depend on trial B's slope priors, on the target population, on covariate overlap, on the outcome model, and on plausible differences between the trials.
@cue(diagnostic = 5)
The package records how many posterior draws it could actually use. Keep that count in your report. A survival median that falls beyond the time grid is a separate issue, reported as the chance that the median was not reached. Never quietly drop an inconvenient check, or present an incomplete summary as complete.
@cue(diagnostic = 6)
Finally, a better predictive score, such as L O O, only says which model predicts these observations better. It cannot tell you whether a hidden difference between the trials exists.
@pause(prompt: "Choose each problem, guess the next step, then reveal the explanation.")

@chapter(Report it well)
@cue(scene = "practice")
@cue(question = 0)
@clear(board)
@board(report: "State the target. Defend the assumptions. Show the sensitivity.")

The point of M L U M R is one explicit model that links individual patient data with published summaries. It lets you ask what both treatments would do in a population you specify, as long as the model and its assumptions hold.

The research on this method describes extensions to more than two treatments. The package covered here compares one index treatment with one comparator. It does not fit networks of many treatments, or random study effects. Some function names look like those in the multi N M A package, but the models and the fitted objects are not interchangeable.

Before you report an analysis, name the treatments, the outcome, the target population and the effect scale. Explain where the data came from, how well the covariates overlap, and that the subgroups do not overlap. Say which covariate distributions and priors you used, and whether slopes were shared or separate. Show the sampling checks, the integration checks and the sensitivity analyses. For survival, state every time and every horizon.

The chart shows what a good report gives: an estimate, its interval, and the population it applies to. To go further, the package website has an article for each outcome type, and the package ships four example datasets, on psoriasis, multiple myeloma, shoulder surgery and tooth decay. Test yourself with the five questions. Then revisit any chapter, and try to predict what will change before you move a control.
@pause(prompt: "Answer the five questions. Then pick a chapter whose chart you can now explain in your own words.")
