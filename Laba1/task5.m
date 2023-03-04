
function task5(step)
    x = -5: step : 5;
    plot(x, task4(x), 'b--o');
    title('task5')
    xlabel('x')
    ylabel('y = 0.5 * x^3 - 10 * x^2 - 10')
    grid on;
    
end