package com.cardano.explorer.specification;

import org.springframework.data.jpa.domain.Specification;

public abstract class BaseSpecification<T, U> {

  public abstract Specification<T> getFilter(U request);

}
