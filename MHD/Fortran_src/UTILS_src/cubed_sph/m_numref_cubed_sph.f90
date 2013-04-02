!
!      module m_numref_cubed_sph
!
      module m_numref_cubed_sph
!
!      Written by H. Matsui on Apr., 2006
!
      use m_precision
!
      implicit none
!
      integer(kind = kint) :: iflag_domain_shell = 1
      integer(kind = kint) :: iflag_mesh =         2
      integer(kind = kint) :: iflag_quad =         1
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
      integer(kind = kint) :: max_coarse_level
      integer(kind = kint), allocatable :: icoarse_level(:,:)
      integer(kind = kint), allocatable :: nstep_coarse(:,:)
!
      integer(kind = kint) :: n_hemi_c, nr_c
      integer(kind = kint) :: n_hemi_fc
!
      integer(kind = kint) :: nskip_s, nskip_r, nl_s, nl_r
      integer(kind = kint) :: nskip_fs, nskip_fr
      integer(kind = kint) :: nl_3, nl_shell
!
      integer(kind = kint) :: numnod_coarse, numele_coarse
      integer(kind = kint) :: nnod_cube_c, nnod_sf_c
      integer(kind = kint) :: nele_cube_c, nele_sf_c, nele_shell_c
      integer(kind = kint) :: nnod_cube_fc, nnod_sf_fc
      integer(kind = kint) :: nele_cube_fc, nele_sf_fc
!
!
      integer(kind = kint) :: n_vert_c
!
!      subroutine allocate_1d_position
!      subroutine allocate_coarsing_parameter
!
!      subroutine deallocate_1d_position
!      subroutine deallocate_coarsing_parameter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine allocate_1d_position
!
      allocate (x_node(num_hemi+1))
      allocate (x_edge(num_hemi))
      allocate (v_node(ncube_vertical+1))
      allocate (v_edge(ncube_vertical))
!
      x_node = 0.0d0
      x_edge = 0.0d0
      v_node = 0.0d0
      v_edge = 0.0d0
!
      end subroutine allocate_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine allocate_coarsing_parameter
!
      allocate(icoarse_level(max_coarse_level,2))
      allocate(nstep_coarse(0:max_coarse_level,2))
      icoarse_level = 1
      nstep_coarse = 1
!

      end subroutine allocate_coarsing_parameter
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine deallocate_1d_position
!
      deallocate (x_node, v_node)
      deallocate (x_edge, v_edge)
!
      end subroutine deallocate_1d_position
!
!   --------------------------------------------------------------------
!
      subroutine deallocate_coarsing_parameter
!
      deallocate(icoarse_level)
      deallocate(nstep_coarse)
!

      end subroutine deallocate_coarsing_parameter
!
!   --------------------------------------------------------------------
!
      end module m_numref_cubed_sph
