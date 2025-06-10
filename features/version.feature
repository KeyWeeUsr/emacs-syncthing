Feature: Connect to already online Syncthing server
  Background:
    Given I override default base URL

  Scenario Outline: Can connect after token input
    Given Server "<version>" is running in the background
    And I am running client in "<mode>" mode

    When I have no API token set
    And I launch client "<fail>"

    Then client redirects to token customization

    When I set a API token in "<how>" to "<token>"
    And I launch client "<second-launch>" or skip
    Then client launches new buffer
    And client buffer header contains "<header contains>"
    And client buffer contains default folder

    Examples:
      |     version | mode            | token    | how         | second-launch | header contains |
      |      1.18.0 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.18.0 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.26.0 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      |      1.29.6 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <customize> | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <customize> | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <customize> | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <customize> | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <manual>    | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <manual>    | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <manual>    | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <manual>    | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <setq>      | <success>     | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <setq>      | <fail>        | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <setq>      | <success>     | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <setq>      | <fail>        | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <setq>      | <success>     | <version>       |

  Scenario Outline: Can connect with a pre-configured key
    Given Server "<version>" is running in the background
    And I am running client in "<mode>" mode

    When I set a API token in "<how>" to "<token>"
    And I launch client "<launch>" or skip
    Then client launches new buffer
    And client buffer header contains "<header contains>"
    And client buffer contains default folder

    Examples:
      |     version | mode            | token    | how      | launch    | header contains |
      |      1.18.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.18.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.18.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.18.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |      1.18.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.18.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.18.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.18.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.26.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.26.0 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.26.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |      1.26.0 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.26.0 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.26.0 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.26.0 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.29.6 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      |      1.29.6 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <manual> | <success> | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.29.6 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      |      1.29.6 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.29.6 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      |      1.29.6 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      |      1.29.6 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <manual> | <success> | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <manual> | <fail>    | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <manual> | <success> | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <manual> | <fail>    | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <manual> | <success> | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <setq>   | <success> | <version>       |
      | 2.0.0-rc.18 | non-interactive | <empty>  | <setq>   | <fail>    | <empty>         |
      | 2.0.0-rc.18 | non-interactive | <apikey> | <setq>   | <success> | <version>       |
      | 2.0.0-rc.18 | interactive     | <empty>  | <setq>   | <fail>    | <empty>         |
      | 2.0.0-rc.18 | interactive     | <apikey> | <setq>   | <success> | <version>       |
