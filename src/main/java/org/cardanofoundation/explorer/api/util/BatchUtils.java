package org.cardanofoundation.explorer.api.util;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;

import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;

@UtilityClass
@Slf4j
public class BatchUtils {

  public static <T> BigInteger processInBatches(
      int batchSize,
      List<T> itemList,
      Function<Collection<T>, CompletableFuture<BigInteger>> processingFunction) {

    List<CompletableFuture<BigInteger>> futures = new ArrayList<>();
    final int COLLECTION_SIZE = itemList.size();
    for (int startBatchIdx = 0; startBatchIdx < COLLECTION_SIZE; startBatchIdx += batchSize) {
      int endBatchIdx = Math.min(startBatchIdx + batchSize, COLLECTION_SIZE);
      final List<T> batchList = itemList.subList(startBatchIdx, endBatchIdx);
      futures.add(processingFunction.apply(batchList));
    }

    CompletableFuture<BigInteger> allFutures =
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0]))
            .thenApply(
                v ->
                    futures.stream()
                        .map(CompletableFuture::join)
                        .filter(Objects::nonNull)
                        .reduce(BigInteger::add)
                        .orElse(BigInteger.ZERO));

    try {
      return allFutures.get();
    } catch (InterruptedException | ExecutionException e) {
      return null;
    }
  }
}
