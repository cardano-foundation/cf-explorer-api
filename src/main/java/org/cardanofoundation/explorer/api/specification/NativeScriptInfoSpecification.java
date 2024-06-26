package org.cardanofoundation.explorer.api.specification;

import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.criteria.Predicate;

import org.springframework.data.jpa.domain.Specification;

import org.cardanofoundation.explorer.api.model.request.script.nativescript.NativeScriptFilterRequest;
import org.cardanofoundation.explorer.common.entity.explorer.NativeScriptInfo;
import org.cardanofoundation.explorer.common.entity.explorer.NativeScriptInfo_;

public class NativeScriptInfoSpecification {

  public static Specification<NativeScriptInfo> filter(
      Long currentSlot, NativeScriptFilterRequest filterRequest) {
    return (root, query, cb) -> {
      final List<Specification<NativeScriptInfo>> predicates = new ArrayList<>();
      if (filterRequest.getOpenTimeLocked() != null) {
        predicates.add(filterByTimeLock(currentSlot, filterRequest.getOpenTimeLocked()));
      }
      if (filterRequest.getIsMultiSig() != null) {
        predicates.add(filterByIsMultiSig(filterRequest.getIsMultiSig()));
      }
      return cb.and(
          predicates.stream()
              .map(item -> item.toPredicate(root, query, cb))
              .toArray(Predicate[]::new));
    };
  }

  public static Specification<NativeScriptInfo> filterByTimeLock(
      Long currentSlot, Boolean openTimeLocked) {
    return (root, query, cb) -> {
      if (openTimeLocked == null) {
        return null;
      } else {
        Predicate afterSlotIsNullPredicate = cb.isNull(root.get(NativeScriptInfo_.AFTER_SLOT));
        Predicate beforeSlotIsNullPredicate = cb.isNull(root.get(NativeScriptInfo_.BEFORE_SLOT));
        Predicate afterSlotIsLessThanCurrentSlotPredicate =
            cb.lessThan(root.get(NativeScriptInfo_.AFTER_SLOT), currentSlot);
        Predicate beforeSlotIsGreaterThanCurrentSlotPredicate =
            cb.greaterThan(root.get(NativeScriptInfo_.BEFORE_SLOT), currentSlot);
        Predicate afterSlotIsLessThanCurrentSlotAndBeforeSlotIsGreaterThanCurrentSlotPredicate =
            cb.and(
                afterSlotIsLessThanCurrentSlotPredicate,
                beforeSlotIsGreaterThanCurrentSlotPredicate);
        Predicate afterSlotIsNullAndBeforeSlotIsGreaterThanCurrentSlotPredicate =
            cb.and(afterSlotIsNullPredicate, beforeSlotIsGreaterThanCurrentSlotPredicate);
        Predicate afterSlotIsLessThanCurrentSlotAndBeforeSlotIsNullPredicate =
            cb.and(afterSlotIsLessThanCurrentSlotPredicate, beforeSlotIsNullPredicate);
        Predicate afterSlotIsNullAndBeforeSlotIsNullPredicate =
            cb.and(afterSlotIsNullPredicate, beforeSlotIsNullPredicate);
        Predicate openTimeLockedPredicate =
            cb.or(
                afterSlotIsLessThanCurrentSlotAndBeforeSlotIsGreaterThanCurrentSlotPredicate,
                afterSlotIsNullAndBeforeSlotIsGreaterThanCurrentSlotPredicate,
                afterSlotIsLessThanCurrentSlotAndBeforeSlotIsNullPredicate,
                afterSlotIsNullAndBeforeSlotIsNullPredicate);
        if (openTimeLocked) {
          return openTimeLockedPredicate;
        } else {
          return cb.not(openTimeLockedPredicate);
        }
      }
    };
  }

  public static Specification<NativeScriptInfo> filterByIsMultiSig(Boolean isMultiSig) {
    return (root, query, cb) -> {
      if (isMultiSig == null) {
        return null;
      } else if (isMultiSig) {
        return cb.greaterThan(root.get(NativeScriptInfo_.NUMBER_SIG), 1L);
      } else {
        return cb.lessThanOrEqualTo(root.get(NativeScriptInfo_.NUMBER_SIG), 1L);
      }
    };
  }
}
