@Parsing

  Feature: Parsing raw string to markdown

    Scenario: parsing a nested check list
      When I parse a string containing a nested check list
      Then I receive a document containing 1 paragraph
      And the 1st paragraph is a "task list"
      And the 1st paragraph is a task list containing a single item with a single nested item