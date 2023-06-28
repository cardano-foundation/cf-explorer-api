package org.cardanofoundation.explorer.api.controller.validation;

import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;

public class PageZeroValidator implements ConstraintValidator<PageZeroValid, Pagination> {

  @Override
  public boolean isValid(Pagination pagination, ConstraintValidatorContext context) {
    return pagination.getPage() == null || pagination.getPage() == 0;
  }
}
