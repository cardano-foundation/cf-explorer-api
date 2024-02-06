package org.cardanofoundation.explorer.api.controller.validation;

import java.lang.annotation.*;

import jakarta.validation.Constraint;
import jakarta.validation.Payload;

@Constraint(validatedBy = {PageZeroValidator.class})
@Target({ElementType.PARAMETER, ElementType.FIELD})
@Retention(RetentionPolicy.RUNTIME)
@Documented
public @interface PageZeroValid {
  String message() default "The page number must be 0";

  Class<?>[] groups() default {};

  Class<? extends Payload>[] payload() default {};
}
