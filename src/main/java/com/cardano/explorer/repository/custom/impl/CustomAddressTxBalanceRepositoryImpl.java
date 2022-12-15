package com.cardano.explorer.repository.custom.impl;

import com.cardano.explorer.repository.custom.CustomAddressTxBalanceRepository;
import com.sotatek.cardano.common.entity.AddressTxBalance;
import com.sotatek.cardano.common.entity.AddressTxBalance_;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Block_;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Tx_;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import javax.persistence.EntityManager;
import javax.persistence.Query;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Repository;

@Repository
@RequiredArgsConstructor
public class CustomAddressTxBalanceRepositoryImpl implements CustomAddressTxBalanceRepository {
  private final EntityManager entityManager;

  public BigDecimal getBalanceByAddressAndTime(String address, Timestamp time) {
    CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
    CriteriaQuery<AddressTxBalance> criteriaQuery = criteriaBuilder.createQuery(AddressTxBalance.class);
    Root<AddressTxBalance> addressTxBalanceRoot = criteriaQuery.from(AddressTxBalance.class);
    Join<AddressTxBalance, Tx> addressTxBalanceTxJoin = addressTxBalanceRoot.join(AddressTxBalance_.TX);
    Join<Tx, Block> addressTxBalanceTxBlockJoin = addressTxBalanceTxJoin.join(Tx_.BLOCK);
    List<Predicate> predicates = new ArrayList<>();
    if(Boolean.FALSE.equals(StringUtils.isEmpty(address))) {
      predicates.add(
          criteriaBuilder.equal(addressTxBalanceRoot.get(AddressTxBalance_.ADDRESS), address));
    }
    if(Objects.nonNull(time)) {
      predicates.add(
          criteriaBuilder.lessThanOrEqualTo(addressTxBalanceTxBlockJoin.get(Block_.TIME), time));
    }
    criteriaQuery.where(criteriaBuilder.and(predicates.toArray(new Predicate[0])));
    criteriaQuery.select(addressTxBalanceRoot.get(AddressTxBalance_.BALANCE));
    criteriaQuery.orderBy(criteriaBuilder.desc(addressTxBalanceRoot.get(AddressTxBalance_.TX)));
    Query query = entityManager.createQuery(criteriaQuery);
    query.setMaxResults(1);
    var results = query.getResultList();
    return results.isEmpty() ? null : (BigDecimal) results.get(0);
  }
}
