% Matlab Script Normalized Exponential Function
maxCV = 1.0;
a_in = 0:0.01:1;
a_out_lin = 1 * a_in;
a_out_lin_label = 'Linear';
plot_count = 1;

a_out_exp1 = pow2(a_in) - 1.0;
a_out_exp1_label = 'Simple pow2';
plot_count = plot_count + 1;

a_out_exp2 = ( pow2(8 * a_in) - 1.0 ) / 256.0;
a_out_exp2_label = 'pow2 256';
plot_count = plot_count + 1;

% Doesn't work, because a_in(0) == 1 (must be 0)
%a_out_exp3 = ( pow2(8 * (1.0 - a_in)) - 1.0 ) / 256.0;
%a_out_exp3_label = 'a-in - pow2 258';
%plot_count = plot_count + 1;

keep_aspect_ratio = 0;

figure

subplot(plot_count,1,1);
plot(a_in, a_out_lin);
xlabel(a_out_lin_label);
ylabel('A');
if (keep_aspect_ratio == 1)
    pbaspect([1 1 1]);
end

subplot(plot_count,1,2);
plot(a_in, a_out_exp1);
xlabel(a_out_exp1_label);
ylabel('A');
if (keep_aspect_ratio == 1)
    pbaspect([1 1 1]);
end

subplot(plot_count,1,3);
plot(a_in, a_out_exp2);
xlabel(a_out_exp2_label);
ylabel('A');
if (keep_aspect_ratio == 1)
    pbaspect([1 1 1]);
end

% subplot(plot_count,1,4);
% plot(a_in, a_out_exp3);
% xlabel(a_out_exp3_label);
% ylabel('A');
% if (keep_aspect_ratio == 1)
%     pbaspect([1 1 1]);
% end

