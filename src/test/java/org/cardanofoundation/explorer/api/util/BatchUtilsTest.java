package org.cardanofoundation.explorer.api.util;

import static org.junit.jupiter.api.Assertions.*;

import java.math.BigInteger;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.function.Function;

import org.junit.jupiter.api.Test;

class BatchUtilsTest {

  @Test
  void processInBatches_shouldReturnSumOfBatches() throws ExecutionException, InterruptedException {
    List<Integer> itemList = Arrays.asList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
    int batchSize = 3;

    Function<Collection<Integer>, CompletableFuture<BigInteger>> processingFunction =
        batch -> {
          BigInteger sum =
              batch.stream().map(BigInteger::valueOf).reduce(BigInteger.ZERO, BigInteger::add);
          return CompletableFuture.completedFuture(sum);
        };

    BigInteger result = BatchUtils.processInBatches(batchSize, itemList, processingFunction);

    assertEquals(BigInteger.valueOf(55), result);
  }

  @Test
  void processInBatches_shouldHandleEmptyList() throws ExecutionException, InterruptedException {
    List<Integer> itemList = List.of();
    int batchSize = 3;

    Function<Collection<Integer>, CompletableFuture<BigInteger>> processingFunction =
        batch -> {
          BigInteger sum =
              batch.stream().map(BigInteger::valueOf).reduce(BigInteger.ZERO, BigInteger::add);
          return CompletableFuture.completedFuture(sum);
        };

    BigInteger result = BatchUtils.processInBatches(batchSize, itemList, processingFunction);

    assertEquals(BigInteger.ZERO, result);
  }

  @Test
  void processInBatches_shouldReturnNull_whenExceptionIsThrown() {
    List<Integer> itemList = Arrays.asList(1, 2, 3, 4, 5);
    int batchSize = 3;

    Function<Collection<Integer>, CompletableFuture<BigInteger>> processingFunction =
        batch -> {
          throw new RuntimeException("Test exception");
        };

    assertThrows(
        RuntimeException.class,
        () -> BatchUtils.processInBatches(batchSize, itemList, processingFunction));
  }
}
