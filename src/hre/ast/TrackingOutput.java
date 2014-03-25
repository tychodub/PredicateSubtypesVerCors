// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package hre.ast;

import java.io.*;
import java.util.Stack;

/**
 * This class provides the pretty printing classes with a way of
 * keeping track of which AST nodes correspond to which output characters.
 *  
 * @author sccblom
 *
 */
public class TrackingOutput {
  private PrintStream output;
//  private boolean show;

  private static class Frame {
    public int line,col;
    TrackingTree tree;
    public Frame(int line,int col,Origin origin){
      this.line=line;
      this.col=col;
      this.tree=new TrackingTree(origin);
    }
  }
  private Stack<Frame> stack=new Stack();
  private Frame frame;
  private int line=1;
  private int col=1;
  private int indent=0;
  private boolean atnewline=true;
  private boolean closeout;
  
  public TrackingOutput(PrintStream output,boolean closeout){
    this.closeout=closeout;
    this.output=output;
    frame=new Frame(line,col,new MessageOrigin("unknown"));
  }

  public void enter(Origin origin){
    stack.push(frame);
    frame=new Frame(line,col,origin);
  }
  public void leave(Origin origin){
    if (stack.empty()){
      throw new Error("attempt to leave outmost frame");
    }
    Frame parent=stack.pop();
    if (origin != frame.tree.getOrigin()){
      throw new Error("enter/leave mismatch: found "+frame.tree.getOrigin()+", expected: "+origin);
    }
    parent.tree.add(frame.tree,frame.line,frame.col,line,col);
    //System.err.printf("xxx linking line %d col %d line %d col %d with %s\n",frame.line,frame.col,line,col,origin);
    frame=parent;
  }
  public void incrIndent(){ indent+=2; }
  public void decrIndent(){ indent-=2; }
  public void newline(){
//    if(atnewline && show){
//      System.err.printf("%4d ",line);
//    }
    output.println("");
//    if (show) System.err.println("");
    line++;
    atnewline=true;
    col=1;
  }
  public void print(String s){
    if(atnewline){
//      if (show) System.err.printf("%4d ",line);
      for(int i=0;i<indent;i++) {
        output.print(" ");
//        if (show) System.err.print(" ");
      }
      atnewline=false;
      col+=indent;
    }
    output.print(s);
//    if (show) System.err.print(s);
    col+=s.length();
  }
  public void println(String s){
    print(s);
    newline();
  }
  public void printf(String format,Object ... args){
    print(String.format(format,args));
  }
  public void lnprintf(String format,Object ... args){
    print(String.format(format,args));
    newline();
  }
  public TrackingTree close(){
    if (!stack.empty()){
      throw new Error("tracking stack not empty");
    }
    if (closeout) output.close();
    return frame.tree;
  }
}

