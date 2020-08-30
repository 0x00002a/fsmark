
jmp() 
{       
    TARGET=$(fsm fp ${@}) || return 1
    cd $TARGET
    return
}

catm() 
{   
    TARGET=$(fsm fp ${@}) || return 1
    cat $TARGET
    return
}

editm() 
{
    TARGET=$(fsm fp ${@}) || return 1
    $EDITOR "$TARGET"
}

