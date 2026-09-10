import DarkTower.WarMachine.PreferenceRiskWitness
open DarkTower.WarMachine.F10RuledCarrier DarkTower.WarMachine.PreferenceRiskWitness
example : seed.mass () (organisationOutcome .abstained) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .abstained) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .agentUnavailable) = (1 / 8 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .agentUnavailable) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .artifactOnly) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .artifactOnly) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .buildFailed) = (1 / 8 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .buildFailed) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .cancelled) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .cancelled) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .dispatchFailed) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .dispatchFailed) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .groundedChange) = (1 / 2 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .groundedChange) = (1 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .groundedNoChange) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .groundedNoChange) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .guardrailRefusal) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .guardrailRefusal) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .incomplete) = (1 / 8 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .incomplete) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .noSelection) = (1 / 8 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .noSelection) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]

example : seed.mass () (organisationOutcome .substrateUnavailable) = (0 : ℝ) := by norm_num [seed, organisationOutcome]

example : groundedPrediction.mass () (organisationOutcome .substrateUnavailable) = (0 : ℝ) := by norm_num [groundedPrediction, organisationOutcome]
#print axioms concrete_scalarKL
