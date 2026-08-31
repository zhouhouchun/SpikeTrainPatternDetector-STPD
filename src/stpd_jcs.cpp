#ifdef __clang__
# pragma clang diagnostic ignored "-Wunknown-warning-option"
#endif

#include <R.h>
#include <Rinternals.h>

#include <charconv>
#include <cmath>
#include <string>
#include <system_error>

namespace {

std::string normalize_exponent(std::string value) {
  std::size_t pos = value.find_first_of("eE");
  if (pos == std::string::npos) return value;
  value[pos] = 'e';
  std::string mantissa = value.substr(0, pos);
  std::string exponent = value.substr(pos + 1);
  char sign = '+';
  if (!exponent.empty() && (exponent[0] == '+' || exponent[0] == '-')) {
    sign = exponent[0];
    exponent.erase(0, 1);
  }
  std::size_t first = exponent.find_first_not_of('0');
  exponent = first == std::string::npos ? "0" : exponent.substr(first);
  return mantissa + "e" + sign + exponent;
}

std::string ecmascript_number(double value) {
  if (!std::isfinite(value)) return std::string();
  if (value == 0.0) return "0";

  const double absolute = std::fabs(value);
  const std::chars_format format =
      (absolute >= 1e-6 && absolute < 1e21)
          ? std::chars_format::fixed
          : std::chars_format::scientific;
  char buffer[128];
  const auto result = std::to_chars(buffer, buffer + sizeof(buffer), value, format);
  if (result.ec != std::errc()) return std::string();
  std::string out(buffer, result.ptr);
  if (format == std::chars_format::scientific) out = normalize_exponent(out);
  return out;
}

}  // namespace

extern "C" SEXP stpd_jcs_number_c(SEXP values_sexp) {
  if (TYPEOF(values_sexp) != REALSXP) {
    Rf_error("JCS number input must be a double vector.");
  }
  const R_xlen_t n = XLENGTH(values_sexp);
  SEXP output = PROTECT(Rf_allocVector(STRSXP, n));
  for (R_xlen_t i = 0; i < n; ++i) {
    const double value = REAL(values_sexp)[i];
    const std::string encoded = ecmascript_number(value);
    if (encoded.empty()) {
      UNPROTECT(1);
      Rf_error("JCS numbers must be finite binary64 values.");
    }
    SET_STRING_ELT(output, i, Rf_mkCharCE(encoded.c_str(), CE_UTF8));
  }
  UNPROTECT(1);
  return output;
}
