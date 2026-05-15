// Numerical floor used only for generated-quantity ratios. It prevents
// division by zero while preserving the posterior draws themselves.
real safe_divide(real num, real denom) {
  return num / fmax(denom, 1e-10);
}
