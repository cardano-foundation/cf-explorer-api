package com.cardano.explorer.model.request;

import com.sotatek.cardanocommonapi.pagination.Pagination;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DelegationFilterRequest extends Pagination {

  private String search;
}
