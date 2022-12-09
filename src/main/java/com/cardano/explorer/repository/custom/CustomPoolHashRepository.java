package com.cardano.explorer.repository.custom;

import java.util.List;
import org.springframework.data.domain.Pageable;

public interface CustomPoolHashRepository {

  List<Long> findAllPoolHashId(Pageable pageable, String search);

  Long totalPoolHashId(String search);
}
