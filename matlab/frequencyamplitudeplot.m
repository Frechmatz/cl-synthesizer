% Frequency Amplitude Plot
% Input is a CSV file with two columns: frequency and amplitude
% The file must not contain a header row
sampleRate = 44100;
data = csvread('/Users/olli/waves/midi-cc-interface-example-2.csv');

frequencies = data(:, 1);
amplitudes = data(:, 2);

% Create data for X-Axis
ticks = 0:1:size(frequencies)-1;
ticks = ticks / sampleRate;
figure
subplot(2,1,1);
plot(ticks, frequencies);
xlabel('t (s)')
ylabel('f (Hz)')

subplot(2,1,2);
plot(ticks, amplitudes);
xlabel('t (s)')
ylabel('Amplitude (V)')

