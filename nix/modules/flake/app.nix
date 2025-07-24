{ inputs, ... }:
{
  imports = [
    inputs.process-compose-flake.flakeModule
  ];

  perSystem = { self', pkgs, lib, ... }: {
    process-compose."default" = pc: {
      imports = [
        inputs.services-flake.processComposeModules.default
      ];

      # Disable TUI
      cli = {
        # environment.PC_DISABLE_TUI = true;
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

      # Show helpful startup message and add custom processes
      settings = {
        log_level = "info";
        processes = {
          # Todo app (depends on ollama being ready)
          todo-app = {
            command = "${self'.packages.t}/bin/t";
            depends_on = {
              ollama1 = {
                condition = "process_healthy";
              };
            };
            readiness_probe = {
              http_get = {
                host = "127.0.0.1";
                port = 3000;
                path = "/";
              };
              initial_delay_seconds = 2;
              period_seconds = 1;
              timeout_seconds = 2;
              success_threshold = 1;
              failure_threshold = 5;
            };
          };
        };
      };
    };
  };
}
