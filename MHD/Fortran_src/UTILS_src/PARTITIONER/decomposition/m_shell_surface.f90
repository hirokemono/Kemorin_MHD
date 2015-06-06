!
!      module m_shell_surface
!
!     Written by H. Matsui
!
!
!      subroutine allocate_linear_shell
!      subroutine allocate_quad_shell
!      subroutine allocate_local_nnod_shell(nproc)
!
!      subroutine deallocate_linear_shell
!      subroutine deallocate_quad_shell
!      subroutine deallocate_local_nnod_shell
!
      module m_shell_surface
!
      use m_precision
!
      implicit none
!
      integer(kind=kint ) :: nnod_CMB, num_CMB
      integer(kind=kint ) :: nedge_CMB, nele_CMB
      integer(kind=kint ) :: num_layer, nlayer_ICB, nlayer_CMB
      integer(kind=kint ) :: nnod_cube, num_cube, nedge_cube
      integer(kind=kint ) :: nunod_20_CMB
!
      real(kind=kreal),  allocatable :: xx_cmb(:,:)
      real(kind=kreal),  allocatable :: rtp_cmb(:,:)
      real(kind=kreal),  allocatable :: ar_cmb(:), s_cmb(:), as_cmb(:)

      integer(kind=kint),  allocatable :: inod_free(:)

      integer(kind=kint ),  allocatable :: istack_sph(:)
      integer(kind=kint ),  allocatable :: item_sph(:,:)
      integer(kind=kint ),  allocatable :: istack20_sph(:)
      integer(kind=kint ),  allocatable :: item20_sph(:,:)

      integer(kind=kint ), dimension(:), allocatable :: numnod_local
      integer(kind=kint ), dimension(:), allocatable :: ncore_local
      integer(kind=kint ), dimension(:), allocatable :: nrest_local
!
      integer(kind=kint ), dimension(:), allocatable :: numcmb_local

      integer(kind=kint ),  allocatable :: IGROUP_cmb(:)
      integer(kind=kint ),  allocatable :: IGROUP_radius(:)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_linear_shell
!
      allocate ( xx_cmb(num_CMB,3) )
      allocate ( rtp_cmb(num_CMB,3) )
      allocate ( ar_cmb(num_CMB) )
      allocate ( s_cmb(num_CMB) )
      allocate ( as_cmb(num_CMB) )
!
      allocate ( IGROUP_cmb(num_CMB) )
      allocate ( IGROUP_radius(num_layer) )
!
      allocate ( istack_sph(nnod_CMB) )
      allocate ( item_sph(num_layer,nnod_CMB) )
      allocate (inod_free(num_cube))
!
      xx_cmb = 0.0d0
      rtp_cmb = 0.0d0
      ar_cmb = 0.0d0
      s_cmb = 0.0d0
      as_cmb = 0.0d0
!
      IGROUP_cmb = 0
      IGROUP_radius = 0
!
      istack_sph = 0
      item_sph = 0
      inod_free = 0
!
      end subroutine allocate_linear_shell
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_quad_shell
!
      allocate ( istack20_sph(num_CMB) )
      allocate ( item20_sph(num_layer,num_CMB) )
!
      istack20_sph = 0
      item20_sph = 0
!
      end subroutine allocate_quad_shell
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_local_nnod_shell(nproc)
!
      integer(kind = kint), intent(in) :: nproc
!
      allocate (numnod_local(nproc))
      allocate (numcmb_local(nproc))
      allocate (ncore_local (nproc))
      allocate (nrest_local (nproc))
!
      numnod_local = 0
      numcmb_local = 0
      ncore_local =  0
      nrest_local =  0
!
      end subroutine allocate_local_nnod_shell
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine deallocate_linear_shell
!
      deallocate ( xx_cmb )
      deallocate ( rtp_cmb )
      deallocate ( ar_cmb )
      deallocate ( s_cmb )
      deallocate ( as_cmb )
!
      deallocate ( IGROUP_cmb )
      deallocate ( IGROUP_radius )
!
      deallocate ( istack_sph )
      deallocate ( item_sph )
      deallocate (inod_free )
!
      xx_cmb = 0.0d0
      rtp_cmb = 0.0d0
      ar_cmb = 0.0d0
      s_cmb = 0.0d0
      as_cmb = 0.0d0
!
      IGROUP_cmb = 0
      IGROUP_radius = 0
!
      istack_sph = 0
      item_sph = 0
      inod_free = 0
!
      end subroutine deallocate_linear_shell
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_quad_shell
!
      deallocate ( istack20_sph )
      deallocate ( item20_sph )
!
      end subroutine deallocate_quad_shell
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_local_nnod_shell
!
      deallocate (numnod_local)
      deallocate (numcmb_local)
      deallocate (ncore_local )
      deallocate (nrest_local )
!
      end subroutine deallocate_local_nnod_shell
!
!  ---------------------------------------------------------------------
!
      end module m_shell_surface
