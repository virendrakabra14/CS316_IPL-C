#include "type.hh"

Type::Type() {
    base_type = "";
    base_type_size = DEFAULT_SIZE;
    stars = 0;
    arr_indices.clear();
    stars_brackets = 0;
    lvalue = "";
    was_array_before_decay = false;
}

Type::Type(string base) {
    base_type = base;
    stars = 0;
    arr_indices.clear();
    stars_brackets = 0;
    lvalue = "";
    was_array_before_decay = false;
}

string Type::get_type() {
    string type_full = base_type;
    for(int i=0; i<stars; i++) {
        type_full += "*";
    }
    if(stars_brackets>0) {
        type_full += "(";
        for(int i=0; i<stars_brackets; i++) {
            type_full += "*";
        }
        type_full += ")";
    }
    for(string& s:arr_indices) {
        type_full += "["+s+"]";
    }
    return type_full;
}

int Type::get_size() {
    // cout << get_type() << ": ";
    if(base_type=="") {
        cout << "Type::get_size(): base_type not set" << endl;
        exit(1);
    }
    if(this->is_pointer()) {                // stars_brackets>0 covered here
        // cout << DEFAULT_SIZE << endl;
        return DEFAULT_SIZE;
    }
    int size = stars>0 ? DEFAULT_SIZE : base_type_size;     // works here as pointer, float, int
                                                            // all have same size
                                                            // e.g. struct a* b[10] has size ptr_size*10
    for(string& s:arr_indices) {
        int dim;
        try {
            dim = stoi(s);
        }
        catch(const exception& e) {
            cout << "Type::get_size(): stoi exception" << endl;
            exit(1);
        }
        size *= dim;
    }
    // cout << size << endl;
    return size;
}

bool Type::is_array() {
    return (stars_brackets==0 && arr_indices.size()>0);
}

bool Type::is_pointer() {
    return (stars_brackets>0 || (arr_indices.size()==0 && stars>0));
}

bool Type::is_array_or_pointer() {
    return (stars_brackets>0 || arr_indices.size()>0 || stars>0);
}

void Type::decay_array_to_pointer() {
    if(!this->is_array()) return;
    else {
        this->arr_indices.erase(this->arr_indices.begin());
        if(this->arr_indices.size()>0) this->stars_brackets++;
        else this->stars++;
        this->lvalue = "modifiable";        // NOTE: deref(...) might have array type
                                            // handled in dereference()
        this->was_array_before_decay = true;
    }
}

bool Type::dereference() {
    if(stars_brackets>0) {
        stars_brackets--;
        if(stars_brackets == 0 && arr_indices.size()>0) {
            lvalue = "unmodifiable";
        }
        else {
            lvalue = "modifiable";
        }
        return true;
    }
    else if(arr_indices.size()>0) {
        arr_indices.erase(arr_indices.begin());
        if(arr_indices.size() == 0) {
            lvalue = "modifiable";
        }
        else {
            lvalue = "unmodifiable";
        }
        return true;
    }
    else if(stars>0) {
        stars--;
        lvalue = "modifiable";
        return true;
    }
    return false;
}