function print_pos(p, g) {
    console.log('Player at : ', p.x, ',', p.y, ' - Goal at : ', g.x, ',', g.y);
}
// Called every time OnTimer called :D .
function update() {
   
    let player = player_position();
    let goal = goal_position();

    if (goal.x < player.x) {
        move(-1, 0); // left
    } else
    if (goal.x > player.x) {
        move(1, 0); // right
    } else
    if (goal.y < player.y) {
        move(0, 1); // up
    } else
    if (goal.y > player.y) {
        move(0, -1); // down
    }
    print_pos(player, goal);
}
