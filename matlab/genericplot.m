% Generic Plot Y over Time
% Input is a CSV file with one column
% The file must not contain a header row
sampleRate = 44100;
y = csvread('/Users/olli/waves/vco-sweep-plot-vco-phi.csv');
% Create data for X-Axis
ticks = 0:1:size(y)-1;
ticks = ticks / sampleRate;
figure
plot(ticks, y);
xlabel('t (s)')
ylabel('')

