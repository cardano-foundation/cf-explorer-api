package com.cardano.explorer.model.response;

import java.io.Serializable;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BaseFilterResponse<T> implements Serializable {

  private List<T> data;
  private long totalItems;
  private int totalPages;
  private int currentPage;
}
