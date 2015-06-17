!
!      module m_filter_elength
!
!     Written by H. Matsui
!
!       subroutine allocate_ele_length
!       subroutine allocate_nodal_ele_length
!       subroutine allocate_ref_1d_moment
!
!       subroutine deallocate_filter_moments
!
!       subroutine deallocate_ele_length
!       subroutine deallocate_nodal_ele_length
!       subroutine deallocate_ref_1d_moment
!
      module m_filter_elength
!
      use m_precision
      use t_filter_elength
!
      implicit none
!
!
      type(elen_on_ele_type), save ::  elen_n
      type(elen_diffs_type), save ::  diff1_n
!
      type(elen_on_ele_type), save :: elen_1
      type(elen_diffs_type), save ::  diff1_1
      type(elen_diffs_type), save ::  diff2_1
!
      type(filter_config_type), save :: filter_conf1
!

      integer (kind = kint) :: nf_type
      integer (kind = kint) :: isgs_4_div = 1
!   filter function number for time evolution
!         dynamic model: isgs_4_div = 2, other models: isgs_4_div = 1
!
!      character(len=kchara), allocatable :: filter_type(:)
!
!      real(kind=kreal), allocatable :: f_width(:)
!
      integer (kind = kint) :: nnod_filter_mom, nele_filter_mom
!
!      real(kind=kreal), allocatable :: xmom_1d_org(:,:)
!          one dimensional moment in reference frame
!              (direction,filter No,order)
!   filter_conf1%filter_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_ele_length
!
!
      call alloc_elen_on_ele_type(nele_filter_mom, elen_1)
      call alloc_elen_diffs_type(nele_filter_mom,  diff1_1)
      call alloc_elen_diffs_type(nele_filter_mom,  diff2_1)
!
      end subroutine allocate_ele_length
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_nodal_ele_length
!
      call alloc_elen_on_ele_type(nnod_filter_mom, elen_n)
      call alloc_elen_diffs_type(nnod_filter_mom, diff1_n)
!
      end subroutine allocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine allocate_ref_1d_moment
!
      allocate( filter_conf1%filter_type(nf_type) )
      allocate( filter_conf1%f_width(nf_type) )
!
      allocate( filter_conf1%xmom_1d_org(nf_type,0:2) )
!
      filter_conf1%f_width =      0.0d0
      filter_conf1%xmom_1d_org =  0.0d0
!
      end subroutine allocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_filter_moments
!
        call deallocate_ele_length
        call deallocate_nodal_ele_length
        call deallocate_ref_1d_moment
!
       end subroutine deallocate_filter_moments
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ele_length
!
      call dealloc_elen_on_ele_type(elen_1)
      call dealloc_elen_diffs_type(diff1_1)
      call dealloc_elen_diffs_type(diff2_1)
!
       end subroutine deallocate_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_nodal_ele_length
!
!
      call dealloc_elen_on_ele_type(elen_n)
      call dealloc_elen_diffs_type(diff1_n)
!
       end subroutine deallocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ref_1d_moment
!
        deallocate( filter_conf1%filter_type )
        deallocate( filter_conf1%f_width )
!
        deallocate( filter_conf1%xmom_1d_org )
!
       end subroutine deallocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!
      end module m_filter_elength
