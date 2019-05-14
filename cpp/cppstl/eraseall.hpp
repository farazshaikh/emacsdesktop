#include <vector>
#include <list>
#include <deque>
#include <map>
namespace eraseHelper {
   namespace detail {
      struct arrayLike {};
      struct listLike {};
      struct maplike {};

      template <typename T, typename A> struct container_traits;

      template <typename T, typename A>  struct container_traits<std::vector<T,A>> {
         typedef arrayLike category;
      };

      template<typename T, typename A> struct container_traits<std::deque<T,A>> {
         typedef arrayLike category;
      };

      template<typename T, typename A> struct container_traits<std::list<T,A>> {
         typedef arrayLike listLike;
      };

      template<typename T, typename A> struct container_traits<std::list<T,A>> {
         typedef arrayLike listLike;
      };

      template<typename T, typename A> struct container_traits<std::map<T,A>> {
         typedef arrayLike maplike;
      };



      template<typename C, typename X> void eraseElement(C& c, const X& elt, arrayLike) {
         c.erase(c.remove(c.begin(), c.end(), elt), c.end());
      }

      template<typename C, typename Pred> void eraseIf(C& c, const Pred p, arrayLike) {
         c.erase(c.remove_if(c.begin(), c.end(), p), c.end());
      }


      template<typename C, typename X> void eraseElement(C& c, const X& elt, listLike) {
          c.remove(elt);
      }

      template<typename C, typename Pred> void eraseIf(C& c, Pred p, listLike) {
         c.remove_if(elt);
      }





      template<typename C, typename X> void eraseElement(C& c, const X& elt, mapLike) {
         c.remove(elt);
      }

      template<typename C, typename Pred> void eraseIf(C& c, Pred p, mapLike) {
         for  (auto i = c.begin(); i != c.end(); ) {
            if (p(*i)) {
               c.erase(i++);
            } else {
               i++;
            }
         }
      }
   };


   template <typename C, typename T> eraseElement(C& containter,const T& element) {
      detail::eraseElement(container, element, typename detail::container_traits<C>::category());
   }
}
