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
!
      implicit none
!
      integer (kind = kint) :: nf_type
      integer (kind = kint) :: isgs_4_div = 1
!   filter function number for time evolution
!         dynamic model: isgs_4_div = 2, other models: isgs_4_div = 1
!
      character(len=kchara), allocatable :: filter_type(:)
!
      real(kind=kreal), allocatable :: f_width(:)
!
      integer (kind = kint) :: nnod_filter_mom, nele_filter_mom
!
      real(kind=kreal), allocatable :: xmom_1d_org(:,:)
!          one dimensional moment in reference frame
!              (direction,filter No,order)
!
      real(kind=kreal),   allocatable :: elen_dx2_nod(:)
      real(kind=kreal),   allocatable :: elen_dy2_nod(:)
      real(kind=kreal),   allocatable :: elen_dz2_nod(:)
!          ratio of element size at each node (node ID, direction)
      real(kind=kreal),   allocatable :: elen_dxdy_nod(:)
      real(kind=kreal),   allocatable :: elen_dydz_nod(:)
      real(kind=kreal),   allocatable :: elen_dzdx_nod(:)
!          ratio of element size at each node (node ID, direction)
!
      real(kind=kreal),   allocatable :: dxdxi_nod(:)
      real(kind=kreal),   allocatable :: dxdei_nod(:)
      real(kind=kreal),   allocatable :: dxdzi_nod(:)
      real(kind=kreal),   allocatable :: dydxi_nod(:)
      real(kind=kreal),   allocatable :: dydei_nod(:)
      real(kind=kreal),   allocatable :: dydzi_nod(:)
      real(kind=kreal),   allocatable :: dzdxi_nod(:)
      real(kind=kreal),   allocatable :: dzdei_nod(:)
      real(kind=kreal),   allocatable :: dzdzi_nod(:)
!          1st difference of elen_nod
!              (node ID, direction of diffrence)
      real(kind=kreal),   allocatable :: elen_dx2_nod_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dy2_nod_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dz2_nod_dx(:,:)
!          1st difference of elen_nod
!              (node ID, direction of diffrence)
      real(kind=kreal),   allocatable :: elen_dxdy_nod_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dydz_nod_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dzdx_nod_dx(:,:)
!          1st difference of elen_nod
!              (node ID, direction of diffrence)
!
!
      real(kind=kreal),   allocatable :: elen_dx2_ele(:)
      real(kind=kreal),   allocatable :: elen_dy2_ele(:)
      real(kind=kreal),   allocatable :: elen_dz2_ele(:)
!          ratio of element size at each element (element ID, direction)
      real(kind=kreal),   allocatable :: elen_dxdy_ele(:)
      real(kind=kreal),   allocatable :: elen_dydz_ele(:)
      real(kind=kreal),   allocatable :: elen_dzdx_ele(:)
!          ratio of element size at each element (element ID, direction)
!
      real(kind=kreal),   allocatable :: elen_dx2_ele_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dy2_ele_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dz2_ele_dx(:,:)
!          1st difference of elength
!              (element ID, direction of diffrence)
      real(kind=kreal),   allocatable :: elen_dxdy_ele_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dydz_ele_dx(:,:)
      real(kind=kreal),   allocatable :: elen_dzdx_ele_dx(:,:)
!          1st difference of elength
!              (element ID, direction of diffrence)
!
      real(kind=kreal),   allocatable :: elen_dx2_ele_dx2(:,:)
      real(kind=kreal),   allocatable :: elen_dy2_ele_dx2(:,:)
      real(kind=kreal),   allocatable :: elen_dz2_ele_dx2(:,:)
!          2nd difference of elength
!              (element ID, direction of diffrence)
      real(kind=kreal),   allocatable :: elen_dxdy_ele_dx2(:,:)
      real(kind=kreal),   allocatable :: elen_dydz_ele_dx2(:,:)
      real(kind=kreal),   allocatable :: elen_dzdx_ele_dx2(:,:)
!          2nd difference of elength
!              (element ID, direction of diffrence)
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
        allocate( elen_dx2_ele(nele_filter_mom) )
        allocate( elen_dy2_ele(nele_filter_mom) )
        allocate( elen_dz2_ele(nele_filter_mom) )
!
        allocate( elen_dxdy_ele(nele_filter_mom) )
        allocate( elen_dydz_ele(nele_filter_mom) )
        allocate( elen_dzdx_ele(nele_filter_mom) )
!
        allocate( elen_dx2_ele_dx(nele_filter_mom,3) )
        allocate( elen_dy2_ele_dx(nele_filter_mom,3) )
        allocate( elen_dz2_ele_dx(nele_filter_mom,3) )
        allocate( elen_dxdy_ele_dx(nele_filter_mom,3) )
        allocate( elen_dydz_ele_dx(nele_filter_mom,3) )
        allocate( elen_dzdx_ele_dx(nele_filter_mom,3) )
!
        allocate( elen_dx2_ele_dx2(nele_filter_mom,3) )
        allocate( elen_dy2_ele_dx2(nele_filter_mom,3) )
        allocate( elen_dz2_ele_dx2(nele_filter_mom,3) )
        allocate( elen_dxdy_ele_dx2(nele_filter_mom,3) )
        allocate( elen_dydz_ele_dx2(nele_filter_mom,3) )
        allocate( elen_dzdx_ele_dx2(nele_filter_mom,3) )
!
       elen_dx2_ele = 0.0d0
       elen_dy2_ele = 0.0d0
       elen_dz2_ele = 0.0d0
!
       elen_dxdy_ele = 0.0d0
       elen_dydz_ele = 0.0d0
       elen_dzdx_ele = 0.0d0
!
       elen_dx2_ele_dx = 0.0d0
       elen_dy2_ele_dx = 0.0d0
       elen_dz2_ele_dx = 0.0d0
       elen_dxdy_ele_dx = 0.0d0
       elen_dydz_ele_dx = 0.0d0
       elen_dzdx_ele_dx = 0.0d0
!
       elen_dx2_ele_dx2 = 0.0d0
       elen_dy2_ele_dx2 = 0.0d0
       elen_dz2_ele_dx2 = 0.0d0
       elen_dxdy_ele_dx2 = 0.0d0
       elen_dydz_ele_dx2 = 0.0d0
       elen_dzdx_ele_dx2 = 0.0d0
!
       end subroutine allocate_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine allocate_nodal_ele_length
!
        allocate( elen_dx2_nod(nnod_filter_mom) )
        allocate( elen_dy2_nod(nnod_filter_mom) )
        allocate( elen_dz2_nod(nnod_filter_mom) )
!
        allocate( elen_dxdy_nod(nnod_filter_mom) )
        allocate( elen_dydz_nod(nnod_filter_mom) )
        allocate( elen_dzdx_nod(nnod_filter_mom) )
!
        allocate( elen_dx2_nod_dx(nnod_filter_mom,3)  )
        allocate( elen_dy2_nod_dx(nnod_filter_mom,3)  )
        allocate( elen_dz2_nod_dx(nnod_filter_mom,3)  )
        allocate( elen_dxdy_nod_dx(nnod_filter_mom,3) )
        allocate( elen_dydz_nod_dx(nnod_filter_mom,3) )
        allocate( elen_dzdx_nod_dx(nnod_filter_mom,3) )
!
        elen_dx2_nod =      0.0d0
        elen_dy2_nod =      0.0d0
        elen_dz2_nod =      0.0d0
!
        elen_dxdy_nod =     0.0d0
        elen_dydz_nod =     0.0d0
        elen_dzdx_nod =     0.0d0
!
        elen_dx2_nod_dx =   0.0d0
        elen_dy2_nod_dx =   0.0d0
        elen_dz2_nod_dx =   0.0d0
        elen_dxdy_nod_dx =  0.0d0
        elen_dydz_nod_dx =  0.0d0
        elen_dzdx_nod_dx =  0.0d0
!
       end subroutine allocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
       subroutine allocate_ref_1d_moment
!
        allocate( filter_type(nf_type) )
        allocate( f_width(nf_type) )
!
        allocate( xmom_1d_org(nf_type,0:2) )
!
        f_width =      0.0d0
        xmom_1d_org =  0.0d0
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
        deallocate( elen_dx2_ele )
        deallocate( elen_dy2_ele )
        deallocate( elen_dz2_ele )
!
        deallocate( elen_dxdy_ele )
        deallocate( elen_dydz_ele )
        deallocate( elen_dzdx_ele )
!
        deallocate( elen_dx2_ele_dx )
        deallocate( elen_dy2_ele_dx )
        deallocate( elen_dz2_ele_dx )
        deallocate( elen_dxdy_ele_dx )
        deallocate( elen_dydz_ele_dx )
        deallocate( elen_dzdx_ele_dx )
!
        deallocate( elen_dx2_ele_dx2 )
        deallocate( elen_dy2_ele_dx2 )
        deallocate( elen_dz2_ele_dx2 )
        deallocate( elen_dxdy_ele_dx2 )
        deallocate( elen_dydz_ele_dx2 )
        deallocate( elen_dzdx_ele_dx2 )
!
       end subroutine deallocate_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_nodal_ele_length
!
!
        deallocate( elen_dx2_nod )
        deallocate( elen_dy2_nod )
        deallocate( elen_dz2_nod )
!
        deallocate( elen_dxdy_nod )
        deallocate( elen_dydz_nod )
        deallocate( elen_dzdx_nod )
!
        deallocate( elen_dx2_nod_dx )
        deallocate( elen_dy2_nod_dx )
        deallocate( elen_dz2_nod_dx )
        deallocate( elen_dxdy_nod_dx )
        deallocate( elen_dydz_nod_dx )
        deallocate( elen_dzdx_nod_dx )
!
       end subroutine deallocate_nodal_ele_length
!
!  ---------------------------------------------------------------------
!
       subroutine deallocate_ref_1d_moment
!
        deallocate( filter_type )
        deallocate( f_width )
!
        deallocate( xmom_1d_org )
!
       end subroutine deallocate_ref_1d_moment
!
!  ---------------------------------------------------------------------
!
      end module m_filter_elength
