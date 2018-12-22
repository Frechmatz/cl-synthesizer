% Plots all columns (y) over time (x)
% Input is a CSV file with n columns and a header row
content = importdata('/Users/olli/waves/adsr-example-3.csv');
sampleRate = 44100;
% Create X-Axis
ticks = 0:1:size(content.data)-1;
ticks = ticks / sampleRate;
columnCount = length(content.colheaders);
figure
for index = 1:columnCount
    subplot(columnCount,1,index);
    plot(ticks,content.data(:, index));
    xlabel('t (s)');
    ylabel(content.colheaders(index));
end
