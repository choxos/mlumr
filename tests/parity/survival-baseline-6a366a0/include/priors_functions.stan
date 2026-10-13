// Shared prior log-density dispatchers for ML-UMR Stan models.
//
// These helpers branch on an integer distribution code so that Stan data
// dictates the prior family at runtime. Call with `target += ...`.
//
//   dist == 0 : normal
//   dist == 1 : student_t (with df)
//   dist == 2 : exponential (only supported by log_prior_sigma;
//               `scale` is interpreted as 1 / rate so that the user-facing
//               `prior_exponential(rate)` rescales correctly; see priors.R)
//
// `df` is ignored when dist != 1; pass any positive placeholder (the
// Stan data block constrains it to be > 0 regardless).

real log_prior_scalar(real x, real location, real scale, int dist, real df) {
  if (dist == 0) return normal_lpdf(x | location, scale);
  else return student_t_lpdf(x | df, location, scale);
}

real log_prior_vector(vector x, vector location, vector scale, int dist, real df) {
  if (dist == 0) return normal_lpdf(x | location, scale);
  else return student_t_lpdf(x | df, location, scale);
}

real log_prior_sigma(real sigma, real location, real scale, int dist, real df) {
  if (dist == 0) return normal_lpdf(sigma | location, scale);
  else if (dist == 1) return student_t_lpdf(sigma | df, location, scale);
  else return exponential_lpdf(sigma | inv(scale));
}

// Prior on the standardized coordinate z, where a regression coefficient
// is reparameterized as `beta = location + scale .* z`. Because the
// transformation is affine in data, Stan needs no Jacobian, and the
// induced prior on `beta` matches the user-specified family:
//   z ~ std_normal()         => beta ~ normal(location, scale)
//   z ~ student_t(df, 0, 1)  => beta ~ student_t(df, location, scale)
//                               (scale invariance of Student-t).
// This affine reparameterization decouples adaptation from the prior
// scale; HMC sees a unit-scale, zero-centered coordinate, which
// improves sampling when the prior scale is mismatched with the
// posterior scale (common with the default weakly-informative priors).
// Note: intentionally retained as API surface; not called by any current model
// (none use this non-centered std-vector prior form). Kept for extension use.
real log_prior_std_vector(vector z, int dist, real df) {
  if (dist == 0) return std_normal_lpdf(z);
  else return student_t_lpdf(z | df, 0, 1);
}
