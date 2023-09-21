package org.cardanofoundation.explorer.api.model.response;

import java.io.Serializable;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.domain.Page;

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

  public BaseFilterResponse(Page<T> page) {
    this.data = page.getContent();
    this.totalItems = page.getTotalElements();
    this.totalPages = page.getTotalPages();
    this.currentPage = page.getNumber();
  }

  public <S> BaseFilterResponse(Page<S> page, List<T> data) {
    this.data = data;
    this.totalItems = page.getTotalElements();
    this.totalPages = page.getTotalPages();
    this.currentPage = page.getNumber();
  }

  public <S> BaseFilterResponse(List<T> data, long totalItems) {
    this.data = data;
    this.totalItems = totalItems;
  }
}
