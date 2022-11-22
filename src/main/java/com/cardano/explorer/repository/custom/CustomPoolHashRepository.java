package com.cardano.explorer.repository.custom;

import com.cardano.explorer.model.request.DelegationFilterRequest;
import java.util.List;

public interface CustomPoolHashRepository {

  List<Long> findAllPoolHashId(DelegationFilterRequest delegationFilterRequest);
}
