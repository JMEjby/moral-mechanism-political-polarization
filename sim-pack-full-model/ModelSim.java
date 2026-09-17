import java.io.*;
import java.nio.file.*;
import java.util.*;

import org.nlogo.headless.HeadlessWorkspace;

public class ModelSim {
  private static HeadlessWorkspace workspace;
  static {
    try {
      workspace = HeadlessWorkspace.newInstance();
        } 
    catch (Exception e) {
      e.printStackTrace();
        }
  }

  public static void sim(int param_world, float param_dens, int param_cr, float param_distol, 
                          int param_dt_offset, int param_hom, float param_prev, float param_prop, float param_invar,
                          int param_id, int n_ticks) {
    workspace.command("tidy");
    //used functionalities
    workspace.command("set save-data True"); //saving the data
    workspace.command("set movement True"); //agents are allowed to move between ticks
    workspace.command("set past-choices-length 5"); // signals are a length of 5
    workspace.command("set asymmetric-tolerance True"); // liberals have a higher tolerance than conservatives
    workspace.command("set negative-influence False"); // liberals have a higher tolerance than conservatives
    
    // unused functionalities
    workspace.command("set recording False"); //not optimising model view for screen recording
    workspace.command("set save-view False"); //not optimising model view for taking screen images
    //no alteration of initial weights or revision function (for robustness purposes)
    workspace.command("set mf-rev 0");
    workspace.command("set influence-strength 1"); // social influence susceptibility


    // simulated parameters 
    workspace.command("set world-size " + param_world); // world size
    workspace.command("set density " + param_dens); //model density
    workspace.command("set conflict-range " + param_cr + " / 100"); // conflict threshold
    workspace.command("set dissimilarity-tolerance " + param_distol); //disimilarity tolerance (liberals)
    workspace.command("set homophily " + param_hom); // homophily
    workspace.command("set choice-prevalence " + param_prev); // choice prevalence
    workspace.command("set choice-proportion " + param_prop); // choice proportion
    workspace.command("set choice-invariance " + param_invar); // choice invariance
    workspace.command("set dt-offset " + param_dt_offset + " * 2.5"); // conservative offset of liberal dissimilarity tolerance
    
    workspace.command("set sim-id " + param_id); // simulation id
    workspace.command("setup");
    workspace.command("repeat " + n_ticks + " [ go ]") ;
  }
  
  public static void main(String[] args) {
    String localDir = System.getProperty("user.dir");
    // Read all lines from the file
    
    try{
      List<String> lines = Files.readAllLines(Paths.get(localDir + "/curr_gen_sim_params.csv"));

      // Declare a List to store the values of the first line (simParams)
      List<String> simParams = new ArrayList<>();


      // Split the first line by commas (assuming it's a CSV)
      String firstLine = lines.get(0);
      String[] values = firstLine.split(",");

      // Add all values of the first line to the simParams list
     simParams = Arrays.asList(values);


      // Assign params 
      int world = Integer.parseInt(simParams.get(1)); // = world size
      float density = Float.parseFloat(simParams.get(2)); // density
      int cr = Integer.parseInt(simParams.get(3)); // conflict threshold
      float distol = Float.parseFloat(simParams.get(4)); //  dissimilarity tolerance
      int dtOffsett = Integer.parseInt(simParams.get(5)); // dissimilarity tolerance offset
      int homophily = Integer.parseInt(simParams.get(6)); // homophily
      float prev = Float.parseFloat(simParams.get(7)); // choice prevalence
      float prop = Float.parseFloat(simParams.get(8)); // choice proportion
      float invar = Float.parseFloat(simParams.get(9)); // choice invariance
      int simID = Integer.parseInt(simParams.get(0)); // id
      int ticks = Integer.parseInt(simParams.get(10)); //number of ticks 
   
      workspace.open(
        (localDir + "/main-model.nlogo"));
      sim(world, density, cr, distol, dtOffsett, homophily, prev, prop, invar, simID, ticks);
      workspace.dispose();
    }
    catch(Exception ex) {
      ex.printStackTrace();
    }
  }
}