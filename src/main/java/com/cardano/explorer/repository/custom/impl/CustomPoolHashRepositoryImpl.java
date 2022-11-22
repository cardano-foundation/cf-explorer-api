package com.cardano.explorer.repository.custom.impl;

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
  public List<Long> findAllPoolHashId(Integer page, Integer size, String search) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<PoolOfflineData> cq = cb.createQuery(PoolOfflineData.class);
    Root<PoolOfflineData> poolOffR = cq.from(PoolOfflineData.class);
    Join<PoolOfflineData, PoolHash> poolHashJoin = poolOffR.join("pool");
    List<Predicate> predicates = new ArrayList<>();
    if (Boolean.FALSE.equals(StringUtils.isNullOrEmpty(search))) {
      predicates.add(
          cb.like(cb.lower(poolOffR.get("json")), PREFIX_POOL_NAME + search.toLowerCase() + "%"));
      if (Boolean.TRUE.equals(StringUtils.isNumeric(search))) {
        predicates.add(cb.equal(poolHashJoin.get("id"), search));
      }
    }
    if (!predicates.isEmpty()) {
      cq.where(cb.or(predicates.toArray(new Predicate[0])));
    }
    cq.select(poolHashJoin.get("id"));
    cq.groupBy(poolHashJoin.get("id"));
    cq.orderBy(cb.asc(poolHashJoin.get("id")));
    Query query = entityManager.createQuery(cq);
    query.setFirstResult((page - 1) * size);
    query.setMaxResults(size);
    List<Long> poolIds = query.getResultList();
    return poolIds;
  }
}
