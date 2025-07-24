{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    process-compose."services" = pc: {
      imports = [
        inputs.services-flake.processComposeModules.default
      ];

      # Disable TUI 
      cli = {
        environment.PC_DISABLE_TUI = true;
        options.no-server = true;
      };

      services =
        let
          dataDirBase = "$HOME/.services-flake/t";
        in
        {
          # Ollama service for LLM inference
          ollama."ollama1" = {
            enable = true;
            dataDir = "${dataDirBase}/ollama";
            models = [ "llama3.2" ];
            host = "127.0.0.1";
            port = 11434;
          };
        };
    };
  };
}
