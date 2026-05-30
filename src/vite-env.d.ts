/// <reference types="vite/client" />

declare module "tinystan" {
  export type TinyStanModel = {
    stanVersion(): string;
    sample(config: Record<string, unknown>): {
      draws: number[][];
      paramNames: string[];
    };
  };

  const StanModel: {
    load(
      modelModule: unknown,
      printCallback?: (message: string) => void,
      errorCallback?: (message: string) => void
    ): Promise<TinyStanModel>;
  };

  export default StanModel;
}
