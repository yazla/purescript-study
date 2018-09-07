var verify = require("zb-email-verifier").verify;

exports.verifyEmail_ = function(params) {
    return verify(params);
}