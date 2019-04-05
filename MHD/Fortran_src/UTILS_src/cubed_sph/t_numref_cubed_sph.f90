!
!      module t_numref_cubed_sph
!
!      Written by H. Matsui on Apr., 2006
!
!!      subroutine alloc_1d_position(csph_p)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!      subroutine alloc_coarsing_parameter(course_p)
!!        type(coarse_cubed_sph), intent(inout) :: course_p
!!
!!      subroutine dealloc_1d_position(csph_p)
!!        type(numref_cubed_sph), intent(inout) :: csph_p
!!      subroutine dealloc_coarsing_parameter(course_p)
!!        type(coarse_cubed_sph), intent(inout) :: course_p
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
        integer(kind = kint) :: iflag_domain_shell = 1
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
      end type numref_cubed_sph
!
!
      type coarse_cubed_sph
        integer(kind = kint) :: max_coarse_level
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
      end type coarse_cubed_sph
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine alloc_1d_position(csph_p)
!
      type(numref_cubed_sph), intent(inout) :: csph_p
!
!
      allocate(csph_p%x_node(csph_p%num_hemi+1))
      allocate(csph_p%x_edge(csph_p%num_hemi))
      allocate(csph_p%v_node(csph_p%ncube_vertical+1))
      allocate(csph_p%v_edge(csph_p%ncube_vertical))
!
      csph_p%x_node = 0.0d0
      csph_p%x_edge = 0.0d0
      csph_p%v_node = 0.0d0
      csph_p%v_edge = 0.0d0
!
      end subroutine alloc_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine alloc_coarsing_parameter(course_p)
!
      type(coarse_cubed_sph), intent(inout) :: course_p
!
!
      allocate(course_p%icoarse_level(course_p%max_coarse_level,2))
      allocate(course_p%nstep_coarse(0:course_p%max_coarse_level,2))
      course_p%icoarse_level = 1
      course_p%nstep_coarse = 1
!
      end subroutine alloc_coarsing_parameter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine dealloc_1d_position(csph_p)
!
      type(numref_cubed_sph), intent(inout) :: csph_p
!
!
      deallocate(csph_p%x_node, csph_p%v_node)
      deallocate(csph_p%x_edge, csph_p%v_edge)
!
      end subroutine dealloc_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine dealloc_coarsing_parameter(course_p)
!
      type(coarse_cubed_sph), intent(inout) :: course_p
!
!
      deallocate(course_p%icoarse_level)
      deallocate(course_p%nstep_coarse)
!
      end subroutine dealloc_coarsing_parameter
!
!   --------------------------------------------------------------------
!
      end module t_numref_cubed_sph
