const getAnagrams = dictionary => (inputWord) => {
  const inputWordFootprint = getWordFootprint(inputWord);
  return dictionary.filter(word => (
    inputWord.length === word.length && footprintEquals(getWordFootprint(word), inputWordFootprint)
  ));
};

const addLetterToFootprint = (footprint, letter) => ({
  ...footprint,
  [letter]: letter in footprint ? footprint.letter + 1 : 1,
});

const getWordFootprint = word => word.split('').reduce(addLetterToFootprint, {});

// We know both words are the same length, so the test below is safe
const footprintEquals = (a, b) => Object.keys(a).every(key => a[key] === b[key]);

const dictionary = [
  'friend',
  'redfin',
  'abcsde',
  'refind',
  'friende',
  'asdasdsa',
  'asdgadgad',
];
const getAnagramsFromDict = getAnagrams(dictionary);
getAnagramsFromDict('friend');
// [ 'friend', 'redfin', 'refind' ]
