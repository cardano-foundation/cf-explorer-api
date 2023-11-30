package org.cardanofoundation.explorer.api.model.response;

import java.io.Serializable;
import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;

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
  private Boolean isDataOverSize;

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

  public <S> BaseFilterResponse(List<T> data, long totalItems, int totalPages, int currentPage) {
    this.data = data;
    this.totalItems = totalItems;
    this.totalPages = totalPages;
    this.currentPage = currentPage;
  }

  public BaseFilterResponse(Page<T> page, boolean isDataOverSize) {
    this.data = page.getContent();
    this.totalItems = page.getTotalElements();
    this.totalPages = page.getTotalPages();
    this.currentPage = page.getNumber();
    this.isDataOverSize = isDataOverSize;
  }

  public static <T> Page<T> getPageImpl(List<T> lst, Pageable pageable) {
    final int start = (int) pageable.getOffset();
    final int end = Math.min((start + pageable.getPageSize()), lst.size());
    if(start > lst.size()) {
      return Page.empty();
    }
    return new PageImpl<>(lst.subList(start, end), pageable, lst.size());
  }
}
