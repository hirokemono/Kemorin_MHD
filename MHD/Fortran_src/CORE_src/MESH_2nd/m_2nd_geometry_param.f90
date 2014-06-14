!
!   module   m_2nd_geometry_param
!>   number of node, element, surface, and edge data
!>   and SMP stack for second mesh
!
!   Written by H. Matsui on Aug., 2006
!
      module m_2nd_geometry_param
!
      use m_precision
!
      implicit  none
!
!>     number of node on local PE (include external node)
      integer( kind=kint )  ::  nnod_2nd
!>     number of node on local PE
      integer( kind=kint )  ::  internal_nod_2nd
!
!>     smp stack for total node on  local PE
      integer( kind=kint ), pointer :: inod_smp_stack_2nd(:)
!>     smp stack for internal node on  local PE
      integer( kind=kint ), pointer :: inter_smp_stack_2nd(:)
!>     maximum number of smp node on local PE
      integer( kind=kint )  ::  maxnod_4_smp_2nd = 0
!>     maximum number of smp internal node on local PE
      integer( kind=kint )  ::  max_in_nod_4_smp_2nd = 0
!
!
      end module m_2nd_geometry_param
