% Frequency Plot
% Input is a CSV file with one frequency column
% The file must not contain a header row
sampleRate = 44100;
frequencies = csvread('/Users/olli/waves/vco-example-2.csv');
% Create data for X-Axis
ticks = 0:1:size(frequencies)-1;
ticks = ticks / sampleRate;
figure
plot(ticks, frequencies);
xlabel('t (s)')
ylabel('f (Hz)')

