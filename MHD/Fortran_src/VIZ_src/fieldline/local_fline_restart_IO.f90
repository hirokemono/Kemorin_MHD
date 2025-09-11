!>@file   local_fline_restart_IO.f90
!!@brief  module local_fline_restart_IO
!!
!!@author H.Matsui
!!@date      Programmed in June, 2024
!
!>@brief  tracer or field line data in each domain
!!
!!@verbatim
!!      subroutine copy_local_tracer_to_IO(fline_lc, particle_IO)
!!        type(local_fieldline), intent(in) :: fline_lc
!!        type(surf_edge_IO_file), intent(inout) :: particle_IO
!!      subroutine copy_local_tracer_from_IO(particle_IO, fline_lc)
!!        type(surf_edge_IO_file), intent(in) :: particle_IO
!!        type(local_fieldline), intent(inout) :: fline_lc
!!
!!      character(len=ilen_hd_particle_connect)                         &
!!     &                         function hd_particle_connect()
!!      character(len=ilen_hd_particle_velocity)                        &
!!     &                         function hd_particle_velocity()
!!      character(len=ilen_hd_particle_marker)                          &
!!     &                         function hd_particle_marker()
!!@endverbatim
!
      module local_fline_restart_IO
!
      use m_precision
      use m_constants
      use t_local_fline
      use t_read_mesh_data
      use calypso_mpi
!
      implicit  none
!
!>      length of hd_particle_connect
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_connect = 1+25+25+33+1+5 
!>      length of ilen_hd_particle_marker
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_velocity = 1+25+1+3
!>      length of hd_particle_marker
      integer(kind = kint), parameter                                   &
     &                   :: ilen_hd_particle_marker = 1+22+1+3
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_tracer_to_IO(fline_lc, particle_IO)
!
      use set_nnod_4_ele_by_type
!
      type(local_fieldline), intent(in) :: fline_lc
      type(surf_edge_IO_file), intent(inout) :: particle_IO
!
      integer(kind = kint) :: i
!
      particle_IO%comm%num_neib = 0
      call alloc_neighbouring_id(particle_IO%comm)
!
      particle_IO%node%numnod =        fline_lc%nnod_line_l
      particle_IO%node%internal_node = fline_lc%nnod_line_l
      call alloc_node_geometry_base(particle_IO%node)
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        particle_IO%node%inod_global(i) = fline_lc%iglobal_fline(i)
        particle_IO%node%xx(i,1) = fline_lc%xx_line_l(1,i)
        particle_IO%node%xx(i,2) = fline_lc%xx_line_l(2,i)
        particle_IO%node%xx(i,3) = fline_lc%xx_line_l(3,i)
      end do
!$omp end parallel do
!
      particle_IO%ele%numele =     fline_lc%nnod_line_l
      particle_IO%ele%nnod_4_ele = 2
!
      call alloc_element_types(particle_IO%ele)
      call alloc_ele_connectivity(particle_IO%ele)
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        particle_IO%ele%iele_global(i) = my_rank
        particle_IO%ele%nodelm(i) = particle_IO%ele%nnod_4_ele
        particle_IO%ele%elmtyp(i)                                       &
     &       = linear_eletype_from_num(particle_IO%ele%nnod_4_ele)
!
        particle_IO%ele%ie(i,1) = fline_lc%iedge_line_l(1,i)
        particle_IO%ele%ie(i,2) = fline_lc%iedge_line_l(2,i)
      end do
!$omp end parallel do
!
      end subroutine copy_local_tracer_to_IO
!
!  ---------------------------------------------------------------------
!
      subroutine copy_local_tracer_from_IO(particle_IO, fline_lc)
!
      type(surf_edge_IO_file), intent(inout) :: particle_IO
      type(local_fieldline), intent(inout) :: fline_lc
!
      integer(kind = kint) :: i
!
!
      fline_lc%nnod_line_l = particle_IO%node%numnod
      if(fline_lc%nnod_line_l .ge. fline_lc%nnod_line_buf) then
        call raise_local_fline_data(fline_lc)
      end if
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        fline_lc%iglobal_fline(i) = particle_IO%node%inod_global(i)
        fline_lc%xx_line_l(1,i) = particle_IO%node%xx(i,1)
        fline_lc%xx_line_l(2,i) = particle_IO%node%xx(i,2)
        fline_lc%xx_line_l(3,i) = particle_IO%node%xx(i,3)
      end do
!$omp end parallel do
!
      fline_lc%nele_line_l = particle_IO%ele%numele
      if(fline_lc%nele_line_l .ge. fline_lc%nele_line_buf) then
         call raise_local_fline_connect(fline_lc)
      end if
!
!$omp parallel do
      do i = 1, fline_lc%nnod_line_l
        fline_lc%iedge_line_l(1,i) = particle_IO%ele%ie(i,1)
        fline_lc%iedge_line_l(2,i) = particle_IO%ele%ie(i,2)
      end do
!$omp end parallel do
!
      end subroutine copy_local_tracer_from_IO
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      character(len=ilen_hd_particle_connect)                           &
     &                         function hd_particle_connect()
!
      hd_particle_connect                                               &
     &      = '!' // char(10)                                           &
     &     // '!  2    Particle indexing' // char(10)                   &
     &     // '!  2.1   local element ID' // char(10)                   &
     &     // '!     and surface ID in elememnt' // char(10)            &
     &     // '!' // char(10)
!
      end function hd_particle_connect
!
!------------------------------------------------------------------
!
      character(len=ilen_hd_particle_velocity)                          &
     &                         function hd_particle_velocity()
!
      hd_particle_velocity                                              &
     &      = '!' // char(10)                                           &
     &     // '!  3.1  particle velocity' // char(10)                   &
     &     // '!' // char(10)
!
      end function hd_particle_velocity
!
!------------------------------------------------------------------
!
      character(len=ilen_hd_particle_marker)                            &
     &                         function hd_particle_marker()
!
      hd_particle_marker                                                &
     &      = '!' // char(10)                                           &
     &     // '!  3.2   scalar marker' // char(10)                      &
     &     // '!' // char(10)
!
      end function hd_particle_marker
!
!------------------------------------------------------------------
!
      end module local_fline_restart_IO
