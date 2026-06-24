!>@file   m_commute_filter_z.f90
!!        module m_commute_filter_z
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2007
!
!>@brief commutive filter in vertical direction
!!
!!@verbatim
!!@endverbatim
!!
      module m_commute_filter_z
!
      use m_precision
!
      implicit none
!
!
      character(len=kchara) :: filter_z_file_head = 'filter_node_l.0'
      character(len=kchara) :: filter_z_file_name
!
      integer (kind = kint) :: totalele
      integer (kind = kint) :: numfilter
      integer (kind = kint) :: iflag_grid
      integer (kind = kint) :: i_int_z_filter
!
      integer (kind = kint) :: num_filter_z, num_filter_h
      character(len=kchara) :: type_filter_z, type_filter_h
      integer (kind = kint) :: iflag_filter, iflag_filter_h
      real(kind = kreal) :: f_width, f_width_h
!
      integer (kind = kint) :: ncomp_mat
!
      end module m_commute_filter_z
