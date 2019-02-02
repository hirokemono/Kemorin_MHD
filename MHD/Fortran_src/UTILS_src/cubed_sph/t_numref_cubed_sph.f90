!
!      module t_numref_cubed_sph
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine alloc_1d_position(csph_param)
!!      subroutine alloc_coarsing_parameter(csph_param)
!!        type(numref_cubed_sph), intent(inout) :: csph_param
!!
!!      subroutine dealloc_1d_position(csph_param)
!!      subroutine dealloc_coarsing_parameter(csph_param)
!!        type(numref_cubed_sph), intent(inout) :: csph_param
!!
!
      module t_numref_cubed_sph
!
      use m_precision
      use m_geometry_constants
!
      implicit none
!
      integer(kind = kint), parameter :: id_cube_spacing =   1
      integer(kind = kint), parameter :: id_sphere_spacing = 2
!
      type numref_cubed_sph
        integer(kind = kint) :: iflag_mesh =         id_sphere_spacing
        integer(kind = kint) :: iflag_quad =         num_t_linear
!
        integer(kind = kint) :: num_hemi
        integer(kind = kint) :: ncube_vertical
!
        real(kind = kreal) :: cube_size
!
        real(kind = kreal), allocatable :: x_node(:)
        real(kind = kreal), allocatable :: x_edge(:)
        real(kind = kreal), allocatable :: v_node(:)
        real(kind = kreal), allocatable :: v_edge(:)
!
!
        integer(kind = kint) :: max_csph_level
        integer(kind = kint), allocatable :: icoarse_level(:,:)
        integer(kind = kint), allocatable :: nstep_coarse(:,:)
!
        integer(kind = kint) :: n_hemi_c
        integer(kind = kint) :: nr_c
        integer(kind = kint) :: n_hemi_fc
!
        integer(kind = kint) :: nskip_s
        integer(kind = kint) :: nskip_r
        integer(kind = kint) :: nl_s
        integer(kind = kint) :: nl_r
        integer(kind = kint) :: nskip_fs
        integer(kind = kint) :: nskip_fr
        integer(kind = kint) :: nl_3
        integer(kind = kint) :: nl_shell
!
        integer(kind = kint) :: numnod_coarse
        integer(kind = kint) :: numele_coarse
        integer(kind = kint) :: nnod_cube_c
        integer(kind = kint) :: nnod_sf_c
        integer(kind = kint) :: nele_cube_c
        integer(kind = kint) :: nele_sf_c
        integer(kind = kint) :: nele_shell_c
        integer(kind = kint) :: nnod_cube_fc
        integer(kind = kint) :: nnod_sf_fc
        integer(kind = kint) :: nele_cube_fc
        integer(kind = kint) :: nele_sf_fc
!
        integer(kind = kint) :: n_vert_c
      end type numref_cubed_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_1d_position(csph_param)
!
      type(numref_cubed_sph), intent(inout) :: csph_param
!
!
      allocate(csph_param%x_node(csph_param%num_hemi+1))
      allocate(csph_param%x_edge(csph_param%num_hemi))
      allocate(csph_param%v_node(csph_param%ncube_vertical+1))
      allocate(csph_param%v_edge(csph_param%ncube_vertical))
!
      csph_param%x_node = 0.0d0
      csph_param%x_edge = 0.0d0
      csph_param%v_node = 0.0d0
      csph_param%v_edge = 0.0d0
!
      end subroutine alloc_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine alloc_coarsing_parameter(csph_param)
!
      type(numref_cubed_sph), intent(inout) :: csph_param
!
!
      allocate(csph_param%icoarse_level(csph_param%max_csph_level,2))
      allocate(csph_param%nstep_coarse(0:csph_param%max_csph_level,2))
      csph_param%icoarse_level = 1
      csph_param%nstep_coarse = 1
!
      end subroutine alloc_coarsing_parameter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_1d_position(csph_param)
!
      type(numref_cubed_sph), intent(inout) :: csph_param
!
!
      deallocate(csph_param%x_node, csph_param%v_node)
      deallocate(csph_param%x_edge, csph_param%v_edge)
!
      end subroutine dealloc_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_coarsing_parameter(csph_param)
!
      type(numref_cubed_sph), intent(inout) :: csph_param
!
!
      deallocate(csph_param%icoarse_level)
      deallocate(csph_param%nstep_coarse)
!
      end subroutine dealloc_coarsing_parameter
!
!   --------------------------------------------------------------------
!
      end module t_numref_cubed_sph
