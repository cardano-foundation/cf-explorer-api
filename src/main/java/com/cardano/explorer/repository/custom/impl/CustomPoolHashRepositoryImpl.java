package com.cardano.explorer.repository.custom.impl;

import com.cardano.explorer.model.request.DelegationFilterRequest;
import com.cardano.explorer.repository.custom.CustomPoolHashRepository;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class CustomPoolHashRepositoryImpl implements CustomPoolHashRepository {

  private final EntityManager entityManager;

  private static final String PREFIX_POOL_NAME = "{\"name\": \"";

  @Override
  public List<Long> findAllPoolHashId(DelegationFilterRequest delegationFilterRequest) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<PoolOfflineData> cq = cb.createQuery(PoolOfflineData.class);
    Root<PoolOfflineData> poolOffR = cq.from(PoolOfflineData.class);
    Join<PoolOfflineData, PoolHash> poolHashJoin = poolOffR.join("pool");
    List<Predicate> predicates = new ArrayList<>();
    String filter = delegationFilterRequest.getSearch();
    if (Boolean.FALSE.equals(StringUtils.isNullOrEmpty(filter))) {
      predicates.add(
          cb.like(cb.lower(poolOffR.get("json")), PREFIX_POOL_NAME + filter.toLowerCase() + "%"));
      if (Boolean.TRUE.equals(StringUtils.isNumeric(filter))) {
        predicates.add(cb.equal(poolHashJoin.get("id"), filter));
      }
    }
    if (!predicates.isEmpty()) {
      cq.where(cb.or(predicates.toArray(new Predicate[0])));
    }
    cq.select(poolHashJoin.get("id"));
    cq.groupBy(poolHashJoin.get("id"));
    cq.orderBy(cb.asc(poolHashJoin.get("id")));
    Query query = entityManager.createQuery(cq);
    query.setFirstResult(
        (delegationFilterRequest.getPage() - 1) * delegationFilterRequest.getSize());
    query.setMaxResults(delegationFilterRequest.getSize());
    List<Long> poolIds = query.getResultList();
    return poolIds;
  }
}
