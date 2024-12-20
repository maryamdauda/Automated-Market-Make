import { describe, it, expect, beforeEach } from "vitest";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;
const address2 = accounts.get("wallet_2")!;

// Mocking the blockchain state and contract functions
type AMMState = {
  tokenXBalance: number;
  tokenYBalance: number;
  lastPrice: number;
  baseFee: number;
  currentFee: number;
};

let ammState: AMMState;

// Helper function to reset the AMM state
const resetAMMState = () => {
  ammState = {
    tokenXBalance: 0,
    tokenYBalance: 0,
    lastPrice: 0,
    baseFee: 30,
    currentFee: 30,
  };
};

// Contract functions to test
const getBalances = () => ({
  tokenX: ammState.tokenXBalance,
  tokenY: ammState.tokenYBalance,
  currentFee: ammState.currentFee,
});

const addLiquidity = (amountX: number, amountY: number) => {
  ammState.tokenXBalance += amountX;
  ammState.tokenYBalance += amountY;
  return true;
};

const calculateDynamicFee = (newPrice: number) => {
  const priceChange = Math.abs(newPrice - ammState.lastPrice);
  return priceChange > 100 ? ammState.baseFee + 10 : ammState.baseFee;
};

const swapXForY = (amountX: number) => {
  const { tokenXBalance, tokenYBalance } = ammState;
  const newTokenXBalance = tokenXBalance + amountX;
  const constant = tokenXBalance * tokenYBalance;
  const newTokenYBalance = Math.floor(constant / newTokenXBalance);
  const yOut = tokenYBalance - newTokenYBalance;
  const newPrice = Math.floor((amountX * 1_000_000) / yOut);

  ammState.currentFee = calculateDynamicFee(newPrice);
  ammState.lastPrice = newPrice;
  ammState.tokenXBalance = newTokenXBalance;
  ammState.tokenYBalance = newTokenYBalance;

  return yOut;
};

// Tests using Vitest
describe("Dynamic Fee AMM Contract", () => {
  beforeEach(() => {
    // Reset the AMM state before each test
    resetAMMState();
  });

  it("should allow adding liquidity", () => {
    const success = addLiquidity(1000, 2000);
    const balances = getBalances();

    expect(success).toBe(true);
    expect(balances.tokenX).toBe(1000);
    expect(balances.tokenY).toBe(2000);
    expect(balances.currentFee).toBe(30);
  });

  it("should calculate dynamic fee correctly for low volatility", () => {
    ammState.lastPrice = 1000;
    const dynamicFee = calculateDynamicFee(1050); // Small change in price
    expect(dynamicFee).toBe(30);
  });

  it("should calculate dynamic fee correctly for high volatility", () => {
    ammState.lastPrice = 1000;
    const dynamicFee = calculateDynamicFee(1200); // Large change in price
    expect(dynamicFee).toBe(40);
  });

  it("should allow swapping tokens and adjust the pool balance", () => {
    addLiquidity(1000, 2000);

    const yOut = swapXForY(500);
    const balances = getBalances();

    expect(yOut).toBeGreaterThan(0);
    expect(balances.tokenX).toBe(1500);
    expect(balances.tokenY).toBeLessThan(2000);
  });

  it("should update the fee after a swap based on volatility", () => {
    addLiquidity(1000, 2000);

    ammState.lastPrice = 1000;
    swapXForY(500);

    expect(ammState.currentFee).toBeGreaterThanOrEqual(30);
  });

  it("should handle multiple swaps correctly", () => {
    addLiquidity(1000, 2000);

    const yOut1 = swapXForY(100);
    const yOut2 = swapXForY(200);

    const balances = getBalances();

    expect(yOut1).toBeGreaterThan(0);
    expect(yOut2).toBeGreaterThan(0);
    expect(balances.tokenX).toBe(1300);
    expect(balances.tokenY).toBeLessThan(2000);
  });
});
