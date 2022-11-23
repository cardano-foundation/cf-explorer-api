package com.cardano.explorer.repository.custom;

import java.util.List;

public interface CustomPoolHashRepository {

  List<Long> findAllPoolHashId(Integer page, Integer size, String search);

  Long totalPoolHashId(Integer page, Integer size, String search);
}
