package org.cardanofoundation.explorer.api.controller.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

import org.springframework.data.domain.Pageable;

public class PageableTopValidator implements ConstraintValidator<PageableTop, Pageable> {


  @Override
  public boolean isValid(Pageable pageable, ConstraintValidatorContext context) {
    return pageable.getPageNumber() == 0;
  }
}
