@Rendering

Feature: Rendering MdParagraphs into formatted strings

  Scenario: Rendering code
    Given an MdDocument exists containing only MdCode
    When I render the document
    Then I receive an attributed string
    And show me the money