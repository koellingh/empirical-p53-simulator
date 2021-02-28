#ifndef ORG_H
#define ORG_H

#include "emp/math/Random.hpp"
#include "emp/tools/string_utils.hpp"

#include "World.h"

class Organism {
    private:
        double coop_prob;
        double points;
        double p_53;
        double mut_rate;
        double food_count;
        bool does_mutate;
        double init_food;
        double mut_malig;
        double mut_benig;
        bool is_malig;
        emp::Ptr<emp::Random> random;

    public:
    Organism(emp::Ptr<emp::Random> _random, double _coop_prob=0.0, double _points=0.0) :
        random(_random), coop_prob(_coop_prob), points(_points) {;}
    Organism(const Organism &) = default;
    Organism(Organism &&) = default;

    Organism & operator=(const Organism &) = default;
    Organism & operator=(Organism &&) = default;
    bool operator==(const Organism &other) const { return (this == &other);}
    bool operator!=(const Organism &other) const {return !(*this == other);}

    double getCoopProb() {return coop_prob;}
    double getPoints() {return points;}
    // own methods for food count
    double getFoodCount() {return food_count;}
    void setFoodCount(double _in) {food_count = _in;}
    void addFoodCount(double _in) {food_count += _in;}
    void substractFoodCount(double _in) {food_count -= _in;}
    // own methods for p_53
    double getP53() {return p_53;}
    void setP53(double _in) {p_53 = _in;}
    void addP53(double _in) {p_53 += _in;}
    void substractP53(double _in) {p_53 -= _in;}
    
    void setCoopProb(double _in) {coop_prob = _in;}
    void setPoints(double _in) {points = _in;}
    void addPoints(double _in) {points += _in;}
    //Setter functions to pass variables from config to org.h
    void setDoesMutate(bool _in) {does_mutate = _in;}
    void setInitFood(double _in) {init_food = _in;}
    void setMutRate(double _in) {mut_rate = _in;}
    void setMutMalig(double _in) {mut_malig = _in;}
    void setMutBenig(double _in) {mut_benig = _in;}
    //bool getMutMalig() {return is_malig;}
    //mutate returns a boolean so that p53 can check if it should kill the cell, returns true if muated, false if doesnt mutate
    bool mutate() {
        if(does_mutate){
          coop_prob += random->GetRandNormal(0.0, mut_rate);
          if(coop_prob < 0) coop_prob = 0;
          else if (coop_prob > 1) coop_prob = 1;
        }
        //replaced all if statements with probability to mutate to use CheckMutate
        if(does_mutate && CheckMutate(mut_rate, 0.0)){
          int num_mut_type = (random->GetDouble());
          if(num_mut_type < (mut_benig)){
            food_count -= random->GetRandNormal(0.25, 0.25);
            if(food_count < 1) food_count = 1;
            //is_malig = false;
            return true;
          }
          else if (num_mut_type < mut_malig+mut_benig) {
            //is_malig = true;
            food_count += random->GetRandNormal(0.25, 0.25);
            
            return true;
          }
          //is_malig = false;
          return false;
          
        }
        //is_malig = false;
        return false;        
    }
    /*function to check if something will mutate
    used to replacae the large amount of code inside if statements
    will make a number between 0 and 99 and check if the value created is lesser than 100*prob
    */
    bool CheckMutate(double prob, double start){
      random->GetRandNormal(start, 99.0); 
      if(random < (prob*100)) return true;
      else return false;
    }
    //should change to something about food count
    emp::Ptr<Organism> checkReproduction() {
        emp::Ptr<Organism> offspring;
        if(points>=food_count) {
            offspring = new Organism(*this);
            points = 0;
            if(mutate() && p_53>(random->GetDouble())){
              offspring->setP53(-1);
              return offspring;
            }
            //changing offsprings p53 value by +- 0.1(max) but something is weird, more often picks negative values for some reason no clue why
            double offspring_p53 = p_53 + (random->GetRandNormal(0, 0.1));
            if (offspring_p53 > 1) offspring_p53 = 1;
            if (offspring_p53 < 0) offspring_p53 = 0;
            offspring->setP53(offspring_p53);
            offspring->mutate();
            offspring->setPoints(0);
        }
        return offspring;
    }

    void Process(double resources) {
        points += resources;
    }


};
#endif