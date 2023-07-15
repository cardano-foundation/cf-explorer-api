package org.cardanofoundation.explorer.api.controller.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.apache.logging.log4j.util.Strings;
import org.cardanofoundation.explorer.api.common.constant.CommonConstant;
import org.springframework.beans.factory.annotation.Value;

public class StakeKeyLengthValidator implements ConstraintValidator<StakeKeyLengthValid, String> {

    @Value("${application.network}")
    private String network;
    @Override
    public void initialize(StakeKeyLengthValid constraintAnnotation) {
        ConstraintValidator.super.initialize(constraintAnnotation);
    }

    @Override
    public boolean isValid(String value, ConstraintValidatorContext context) {
        int length = this.getStakeKeyLengthByNetwork(network);
        if(Strings.isBlank(value) || value.length() != length) {
            context.buildConstraintViolationWithTemplate("The expected length of the hash is " + length + " characters").addConstraintViolation();
            return false;
        }
        return true;
    }

    private int getStakeKeyLengthByNetwork(String network) {
        if(network.equals(CommonConstant.MAINNET_NETWORK)) {
            return CommonConstant.STAKE_KEY_LENGTH_MAINNET;
        } else {
            return CommonConstant.STAKE_KEY_LENGTH_TESTNET;
        }
    }
}
