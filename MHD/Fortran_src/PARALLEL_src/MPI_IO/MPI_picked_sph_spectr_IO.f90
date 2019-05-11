!>@file   MPI_picked_sph_spectr_IO.f90
!!@brief  module MPI_picked_sph_spectr_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine append_picked_spectrum_file(file_name, picked)
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
      module MPI_picked_sph_spectr_IO
!
      use m_precision
      use m_constants
      use calypso_mpi
      use m_calypso_mpi_IO
!
      use t_calypso_mpi_IO_param
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
!
      implicit  none
!
      integer, parameter, private :: len_fixed = 4*16 + 2*25 + 1
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine append_picked_spectrum_file                            &
     &         (time_d, sph_rj, rj_fld, picked)
!
      use set_parallel_file_name
      use picked_sph_spectr_data_IO
!
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      type(calypso_MPI_IO_params) :: IO_param
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(picked%file_prefix)
      if(my_rank .eq. 0) then
        call open_sph_spec_4_monitor(id_pick_mode, file_name, picked)
        close(id_pick_mode)
      end if
!
      call calypso_mpi_barrier
      call calypso_mpi_append_file_open                                 &
     &   (file_name, nprocs, IO_param%id_rank, IO_param%ioff_gl)
      call calypso_mpi_barrier
!
      call write_picked_specr_mpi(IO_param, time_d, sph_rj, rj_fld,     &
     &    picked, picked%ntot_comp_rj)
!
      call calypso_close_mpi_file(IO_param%id_rank)
!
      end subroutine append_picked_spectrum_file
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine write_picked_specr_mpi(IO_param, time_d,               &
     &          sph_rj, rj_fld, picked, ntot_comp_rj)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(time_data), intent(in) :: time_d
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
!
      integer(kind = kint) :: inum, knum, ipick, ist
!
      integer(kind = kint_gl) :: num
      integer :: ilen_n
      integer(kind = MPI_OFFSET_KIND) :: ioffset
!
      character(len = len_fixed+ntot_comp_rj*25),                       &
     &                                 allocatable :: pickedbuf(:)
      real(kind=kreal), allocatable :: d_rj_out(:)
!
!
      ilen_n = len_fixed + ntot_comp_rj*25
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .gt. 0) then
        ioffset = IO_param%ioff_gl                                      &
     &         + ilen_n * picked%istack_picked_spec_lc(my_rank)
!
        allocate(d_rj_out(ntot_comp_rj))
        allocate(pickedbuf(num))
        ist = 0
        if(picked%idx_out(0,4) .gt. 0) then
          ist = 1
          call pick_degre0_sped_4_monitor                               &
     &       (rj_fld, picked, ntot_comp_rj, d_rj_out)
          pickedbuf(1)                                                  &
     &           = picked_each_mode(time_d%i_time_step, time_d%time,    &
     &                              zero, izero, izero, izero,          &
     &                              ntot_comp_rj, d_rj_out)
        end if
!
        do inum = 1, picked%num_sph_mode_lc
          do knum = 1, picked%num_layer
             ipick = knum + (inum-1) * picked%num_layer
             call pick_single_sph_spec_4_monitor(inum, knum,            &
     &           sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!
             pickedbuf(ipick+ist)                                       &
     &           = picked_each_mode(time_d%i_time_step, time_d%time,    &
     &            picked%radius_gl(knum), picked%id_radius(knum),       &
     &            picked%idx_out(inum,1), picked%idx_out(inum,2),       &
     &            ntot_comp_rj, d_rj_out)
          end do
        end do

        call calypso_mpi_seek_wrt_mul_chara                             &
     &     (IO_param%id_rank, ioffset, ilen_n, num, pickedbuf)
        deallocate(d_rj_out, pickedbuf)
      end if
!
      IO_param%ioff_gl = IO_param%ioff_gl                               &
     &       + ilen_n * picked%istack_picked_spec_lc(nprocs)
!
      end subroutine write_picked_specr_mpi
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      function picked_each_mode                                         &
     &       (i_step, time, radius, kr, l, m, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time, radius
      integer(kind = kint), intent(in) :: kr, l, m
      integer(kind = kint), intent(in) :: ntot_comp_rj
      real(kind = kreal), intent(in) :: d_rj_out(ntot_comp_rj)
!
      character(len = len_fixed+ntot_comp_rj*25) :: picked_each_mode
!
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a37,i4,a17)')                                     &
     &         '(i16,1pe25.14e3, i16,1pe25.14e3,2i16,',                 &
     &           ntot_comp_rj, '(1pE25.14e3), a1)'
      write(picked_each_mode,fmt_txt) i_step, time,                     &
     &          kr, radius, l, m, d_rj_out(1:ntot_comp_rj), char(10)
!
      end function  picked_each_mode
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine pick_degre0_sped_4_monitor                             &
     &         (rj_fld, picked, ntot_comp_rj, d_rj_out)
!
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_rj)
!
!
      call copy_rj_spectrum_4_monitor(picked%idx_out(0,4),              &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, half, d_rj_out)
!
      end subroutine pick_degre0_sped_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine pick_single_sph_spec_4_monitor(inum, knum,             &
     &          sph_rj, rj_fld, picked, ntot_comp_rj, d_rj_out)
!
      integer(kind = kint), intent(in) :: inum, knum
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
      integer(kind = kint), intent(in) :: ntot_comp_rj
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_rj)
!
      integer(kind = kint) :: inod
      real(kind = kreal) :: prod_4_zero
!
!
      if(picked%idx_out(inum,1) .eq. 0) then
        prod_4_zero = 0.5d0
      else
        prod_4_zero = 1.0d0
      end if
!
      inod = picked%idx_out(inum,4)                                     &
     &      + (picked%id_radius(knum) - 1) * sph_rj%nidx_rj(2)
!
      call copy_rj_spectrum_4_monitor                                   &
     &   (inod, rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,      &
     &    rj_fld%istack_component, rj_fld%d_fld, picked%num_field_rj,   &
     &    picked%istack_comp_rj, picked%ifield_monitor_rj,              &
     &    ntot_comp_rj, prod_4_zero, d_rj_out)
!
      end subroutine pick_single_sph_spec_4_monitor
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_spectrum_4_monitor(inod, n_point,              &
     &         num_phys_rj, ntot_phys_rj, istack_phys_comp_rj, d_rj,    &
     &         nfld_monitor, istack_comp_monitor, ifld_monitor,         &
     &         ntot_comp_monitor, prod_degree_zero, d_rj_out)
!
      integer(kind = kint), intent(in) :: inod
      integer(kind = kint), intent(in) :: n_point
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      real(kind=kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint), intent(in) :: nfld_monitor
      integer(kind = kint), intent(in) :: ntot_comp_monitor
      integer(kind = kint), intent(in) :: ifld_monitor(nfld_monitor)
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_comp_monitor(0:nfld_monitor)
      real(kind=kreal), intent(in) :: prod_degree_zero
!
      real(kind=kreal), intent(inout) :: d_rj_out(ntot_comp_monitor)
!
      integer(kind = kint) :: nd, i_fld, j_fld, icou, jcou, ncomp
!
!
      do j_fld = 1, nfld_monitor
        i_fld = ifld_monitor(j_fld)
        ncomp = istack_phys_comp_rj(i_fld)                              &
     &         - istack_phys_comp_rj(i_fld-1)
        icou = istack_phys_comp_rj(i_fld-1)
        jcou = istack_comp_monitor(j_fld-1)
        if(ncomp .eq. 3) then
            d_rj_out(jcou+1) = prod_degree_zero * d_rj(inod,icou+1)
            d_rj_out(jcou+2) = prod_degree_zero * d_rj(inod,icou+3)
            d_rj_out(jcou+3) = prod_degree_zero * d_rj(inod,icou+2)
        else
          do nd = 1, ncomp
            d_rj_out(jcou+nd) = d_rj(inod,icou+nd)
          end do
        end if
      end do
!
      end subroutine copy_rj_spectrum_4_monitor
!
! -----------------------------------------------------------------------
!
      end module MPI_picked_sph_spectr_IO
