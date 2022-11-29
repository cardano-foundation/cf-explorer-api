package com.cardano.explorer.repository.custom.impl;

import com.cardano.explorer.repository.custom.CustomPoolHashRepository;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Delegation;
import com.sotatek.cardano.common.entity.EpochStake;
import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardanocommonapi.utils.StringUtils;
import java.math.BigDecimal;
import java.sql.Date;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import javax.persistence.criteria.Subquery;
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
    Query query = entityManager.createQuery(cq);
    query.setFirstResult((page - 1) * size);
    query.setMaxResults(size);
    List<Long> poolIds = query.getResultList();
    return poolIds;
  }

  @Override
  public Long totalPoolHashId(Integer page, Integer size, String search) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<Long> cq = cb.createQuery(Long.class);
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
    cq.distinct(true);
    cq.select(cb.count(poolHashJoin.get("id")));
    Query query = entityManager.createQuery(cq);
    return (Long) query.getSingleResult();
  }

  @Override
  public List<Date> getFiveLastDateByPool(Long poolId, Integer page, Integer size) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<Date> cq = cb.createQuery(Date.class);
    Root<Delegation> delegationR = cq.from(Delegation.class);
    Join<Delegation, PoolHash> poolHashJoin = delegationR.join("poolHash");
    Join<Delegation, Tx> txJoin = delegationR.join("tx");
    Join<Tx, Block> blockJoin = txJoin.join("block");
    cq.where(cb.equal(poolHashJoin.get("id"), poolId));
    cq.select(blockJoin.get("time").as(Date.class));
    cq.groupBy(blockJoin.get("time").as(Date.class));
    cq.orderBy(cb.desc(blockJoin.get("time").as(Date.class)));
    Query query = entityManager.createQuery(cq);
    query.setFirstResult((page - 1) * size);
    query.setMaxResults(size);
    List<Date> dateList = query.getResultList();
    return dateList;
  }

  @Override
  public Long numberDelegatorsByPoolAndDateTxAndCurrentEpoch(Long poolId, Date time,
      Integer epochNo) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<Long> cq = cb.createQuery(Long.class);
    Root<Delegation> delegationR = cq.from(Delegation.class);
    Join<Delegation, PoolHash> poolHashJoin = delegationR.join("poolHash");
    Join<Delegation, Tx> txJoin = delegationR.join("tx");
    Join<Tx, Block> blockJoin = txJoin.join("block");
    List<Predicate> predicates = new ArrayList<>();
    predicates.add(cb.equal(poolHashJoin.get("id"), poolId));
    predicates.add(cb.equal(blockJoin.get("time").as(Date.class), time));
//    predicates.add(cb.equal(blockJoin.get("epochNo"), epochNo)); //Todo ignore filter current epoch
    cq.where(cb.and(predicates.toArray(new Predicate[0])));
    cq.select(cb.count(delegationR.get("id")));
    Query query = entityManager.createQuery(cq);
    return (Long) query.getSingleResult();
  }

  @Override
  public BigDecimal amountStakeByPoolAndDateTxAndCurrentEpoch(Long poolId, Date time,
      Integer epochNo) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<BigDecimal> cq = cb.createQuery(BigDecimal.class);
    Root<EpochStake> epochStakeR = cq.from(EpochStake.class);
    Subquery<Long> subQuery = cq.subquery(Long.class);
    Root<Delegation> delegationR = subQuery.from(Delegation.class);
    Join<Delegation, PoolHash> poolHashJoin = delegationR.join("poolHash");
    Join<Delegation, Tx> txJoin = delegationR.join("tx");
    Join<Tx, Block> blockJoin = txJoin.join("block");
    List<Predicate> predicates = new ArrayList<>();
    predicates.add(cb.equal(poolHashJoin.get("id"), poolId));
    predicates.add(cb.equal(blockJoin.get("time").as(Date.class), time));
//    predicates.add(cb.equal(blockJoin.get("epochNo"), epochNo)); //Todo ignore filter current epoch
    subQuery.where(cb.and(predicates.toArray(new Predicate[0])));
    subQuery.select(delegationR.get("address").get("id"));
    subQuery.groupBy(delegationR.get("address").get("id"));
    cq.where(epochStakeR.get("addr").get("id").in(subQuery));
    cq.select(cb.sum(epochStakeR.get("amount")));
    Query query = entityManager.createQuery(cq);
    return (BigDecimal) query.getSingleResult();
  }

  @Override
  public Long getMinOrMaxDelegatorsByPoolAndCurrentEpoch(Long poolId, Integer epochNo,
      Integer type) {
    CriteriaBuilder cb = entityManager.getCriteriaBuilder();
    CriteriaQuery<Long> cq = cb.createQuery(Long.class);
    Root<Delegation> delegationR = cq.from(Delegation.class);
    Join<Delegation, PoolHash> poolHashJoin = delegationR.join("poolHash");
    Join<Delegation, Tx> txJoin = delegationR.join("tx");
    Join<Tx, Block> blockJoin = txJoin.join("block");
//    predicates.add(cb.equal(blockJoin.get("epochNo"), epochNo)); //Todo ignore filter current epoch
    cq.where(cb.equal(poolHashJoin.get("id"), poolId));
    cq.groupBy(blockJoin.get("time").as(Date.class));
    if (type == 0) {
      cq.orderBy(cb.asc(cb.count(delegationR.get("id"))));
    } else {
      cq.orderBy(cb.desc(cb.count(delegationR.get("id"))));
    }
    cq.select(cb.count(delegationR.get("id")));
    Query query = entityManager.createQuery(cq);
    query.setMaxResults(1);
    query.setFirstResult(0);
    return (Long) query.getSingleResult();
  }
}
