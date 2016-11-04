REEL_DISTRIBUTION = [
  [:cherry, 2],
  [:empty, 5],
  [:bar, 5],
  [:empty, 5],
  [:seven, 8],
  [:empty, 5],
  [:bar, 5],
  [:empty, 6],
  [:cherry, 2],
  [:empty, 6],
  [:bar_bar, 7],
  [:empty, 6],
  [:cherry, 1],
  [:empty, 6],
  [:bar_bar, 6],
  [:empty, 6],
  [:bar, 6],
  [:empty, 6],
  [:bar_bar_bar, 11],
  [:empty, 11],
  [:jackpot, 2],
  [:empty, 11]
]

REEL = REEL_DISTRIBUTION.inject([]) { |a, e| e[1].times { a << e[0] }; a }

THREE_IN_ROW = {
  jackpot: 1666,
  seven: 300,
  bar_bar_bar: 100,
  bar_bar: 50,
  bar: 25,
  cherry: 12,
  empty: 0
}

class CalculatePayout
  attr_reader :line

  def initialize(line)
    @line = line
  end

  def perform
    if three_in_row?
      THREE_IN_ROW[line.first]
    elsif three_of_any_bar?
      12
    elsif two_cherries?
      6
    elsif one_cherry?
      3
    else
      0
    end
  end

  def three_of_any_bar?
   (line - [:bar, :bar_bar, :bar_bar_bar]).empty?
  end

  def two_cherries?
    line.select { |e| e == :cherry }.size == 2
  end

  def one_cherry?
    line.select { |e| e == :cherry }.size == 1
  end

  def three_in_row?
    line.uniq.size == 1
  end
end

class SlotMachine
  def spin(coins = 1)
    line = 3.times.map { REEL.sample }
    {
      line: line,
      payout: CalculatePayout.new(line).perform * coins
    }
  end
end

require 'rspec'

describe CalculatePayout do
  describe '#perform' do
    it 'returns correct value' do
      line = [:jackpot,:jackpot,:jackpot]
      expected_amount = 1666
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:seven, :seven, :seven]
      expected_amount = 300
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:bar_bar_bar,:bar_bar_bar,:bar_bar_bar]
      expected_amount = 100
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:bar_bar,:bar_bar,:bar_bar]
      expected_amount = 50
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:bar,:bar,:bar]
      expected_amount = 25
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:bar, :bar_bar, :bar_bar_bar]
      expected_amount = 12
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:cherry, :cherry, :cherry]
      expected_amount = 12
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:cherry, :cherry, :bar]
      expected_amount = 6
      expect(described_class.new(line).perform).to eq(expected_amount)
    end

    it 'returns correct value' do
      line = [:cherry, :bar, :seven]
      expected_amount = 3
      expect(described_class.new(line).perform).to eq(expected_amount)
    end
  end
end
