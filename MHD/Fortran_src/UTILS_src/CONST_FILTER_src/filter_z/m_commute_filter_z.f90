!
!      module m_commute_filter_z
!
      module m_commute_filter_z
!
!      Written by Kemorin
!
      use m_precision
!
      implicit none
!
!
      character(len=kchara) :: filter_z_file_head = 'filter_node_l.0'
      character(len=kchara) :: filter_z_file_name
!
      integer (kind = kint) :: totalnod, totalnod_x, totalnod_y
      real(kind = kreal) :: xsize, ysize, zsize
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
      integer (kind = kint) :: nmat_ele
      integer (kind = kint) :: nmat_nod
!
      integer (kind = kint) :: nfilter2_4
      integer (kind = kint) :: nfilter2_3
      integer (kind = kint) :: nfilter2_2
      integer (kind = kint) :: nfilter2_1
      integer (kind = kint) :: nfilter6_1
!
      integer (kind = kint) :: ncomp_norm
      integer (kind = kint) :: ncomp_mat
!
      real(kind = kreal), allocatable :: f_mom(:)
      integer (kind = kint), allocatable :: kcomp_norm(:)
      character(len=kchara), allocatable :: filter_moment_type(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_z_filter_mom_params
!
      allocate( kcomp_norm(ncomp_norm) )
      allocate( f_mom(ncomp_norm) )
      allocate( filter_moment_type(ncomp_norm) )
!
      kcomp_norm = 0
      f_mom = 0.0d0
!
      end subroutine allocate_z_filter_mom_params
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_z_filter_mom_params
!
      deallocate( kcomp_norm )
      deallocate( f_mom )
      deallocate( filter_moment_type )
!
      end subroutine deallocate_z_filter_mom_params
!
!  ---------------------------------------------------------------------
!
      end module m_commute_filter_z
