import unittest

from colcise import is_last, string_to_rows, rows_to_columns


class TestUtilityFunctions(unittest.TestCase):
    def test_is_last_with_last_item(self):
        self.assertTrue(is_last([1, 2, 3], 2))

    def test_is_last_with_empty_list(self):
        self.assertFalse(is_last([], 0))
        self.assertFalse(is_last([], 1))

    def test_string_to_rows(self):
        self.assertEqual(string_to_rows("a\nb\nc"), ['a', 'b'])

    def test_string_to_rows_with_empty_string(self):
        self.assertEqual(string_to_rows(''), [])

    def test_string_to_rows_with_no_newlines(self):
        s = 'no newlines here'
        self.assertEqual(string_to_rows(s), [])

    def test_rows_to_columns(self):
        rows = [
            'separated',
            'by spaces',
            "and,commas"
        ]
        cols = rows_to_columns(rows, delimiter=' ')
        self.assertEqual(cols, [['separated'],
                                ['by', 'spaces'],
                                ['and,commas']])
        cols = rows_to_columns(rows, delimiter=',')
        self.assertEqual(cols, [['separated'],
                                ['by spaces'],
                                ['and', 'commas']])


if __name__ == '__main__':
    unittest.main()
