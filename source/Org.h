#ifndef ORG_H
#define ORG_H

#include "emp/math/Random.hpp"
#include "emp/tools/string_utils.hpp"

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

    void mutate() {
        if(does_mutate){
          coop_prob += random->GetRandNormal(0.0, mut_rate);
          if(coop_prob < 0) coop_prob = 0;
          else if (coop_prob > 1) coop_prob = 1;
        }
        int num_mutate = rand() % 100 + 1;
        if(does_mutate && num_mutate < (mut_rate*100)){
          int num_mut_type = rand() % 100 + 1;
          if(num_mut_type < (mut_benig*100)){
            food_count -= random->GetRandNormal(0.0, 0.5); 
          }
          else if (num_mut_type < (mut_malig*100)) {
            food_count += random->GetRandNormal(0.0, 0.5);
          }
          if(food_count < 1) food_count = 1;
        }        
    }
    //should change to something about food count
    emp::Ptr<Organism> checkReproduction() {
        emp::Ptr<Organism> offspring;
        if(points>=food_count) {
            offspring = new Organism(*this);
            points = 0;
            mutate();
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