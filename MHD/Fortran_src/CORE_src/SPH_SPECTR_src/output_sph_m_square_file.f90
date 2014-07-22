!> @file  output_sph_m_square_file.f90
!!      module output_sph_m_square_file
!!
!! @author  H. Matsui
!! @date Programmed in Feb. 2008
!
!> @brief Output mean square of spectr data
!!
!!@verbatim
!!      subroutine write_total_energy_to_screen(my_rank, istep, time)
!!
!!      subroutine write_sph_vol_ave_file(my_rank, istep, time)
!!      subroutine write_sph_vol_ms_file(my_rank, istep, time)
!!      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time)
!!      subroutine write_sph_layer_ms_file(my_rank, istep, time)
!!
!!      subroutine open_sph_vol_rms_file(id_file, fname_rms, mode_label)
!!      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module output_sph_m_square_file
!
      use m_precision
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
!
      implicit none
!
      integer(kind = kint), parameter :: id_file_rms_l =    31
      integer(kind = kint), parameter :: id_file_rms_m =    32
      integer(kind = kint), parameter :: id_file_rms_lm =   33
      integer(kind = kint), parameter :: id_file_rms =      34
      integer(kind = kint), parameter :: id_file_ave =      43
!
      private :: id_file_ave, id_file_rms
      private :: id_file_rms_l, id_file_rms_m, id_file_rms_lm
!
      private :: write_sph_rms_header
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_total_energy_to_screen(my_rank, istep, time)
!
      use m_phys_labels
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: i, icomp
!
!
      if(my_rank .gt. 0) return
      write(*,'(a10,i10,a10,1pe15.8)',advance='no')                     &
     &            'time step=',istep,'time=',time
!
      do i = 1, num_rms_rj
        if (rms_name_rj(i) .eq. fhd_velo) then
          icomp = istack_rms_comp_rj(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_kin = ', rms_sph_vol(icomp)
          exit
        end if
      end do
!
      do i = 1, num_rms_rj
        if (rms_name_rj(i) .eq. fhd_magne) then
          icomp = istack_rms_comp_rj(i)
          write(*,'(a,1pe15.8)',advance='no')                           &
     &              '  E_mag = ', rms_sph_vol(icomp)
          exit
        end if
      end do
      write(*,*)
!
      end subroutine write_total_energy_to_screen
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ave_file(my_rank, istep, time)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(iflag_volume_ave_sph .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms, '(a,a4)') trim(fhead_ave_vol), '.dat'
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_vol_rms_file                                        &
     &      (id_file_ave, fname_rms, mode_label)
!
      write(id_file_ave,'(i10,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, ave_sph_vol(1:ntot_rms_rj)
      close(id_file_ave)
!
      end subroutine write_sph_vol_ave_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_file(my_rank, istep, time)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      call add_dat_extension(fhead_rms_vol, fname_rms)
      write(mode_label,'(a)') 'EMPTY'
      call open_sph_vol_rms_file(id_file_rms, fname_rms, mode_label)
!
      write(id_file_rms,'(i10,1pe23.14e3,1p200e23.14e3)')               &
     &                 istep, time, rms_sph_vol(1:ntot_rms_rj)
      close(id_file_rms)
!
      end subroutine write_sph_vol_ms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: lm
!
!
      if(my_rank .ne. 0)  return
      if(iflag_volume_rms_spec .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
      write(mode_label,'(a)') 'degree'
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_l, fname_rms, mode_label)
!
      do lm = 0, l_truncation
        write(id_file_rms_l,'(i10,1pe23.14e3,i10,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_l(lm,1:ntot_rms_rj)
      end do
!
      close(id_file_rms_l)
!
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_m.dat'
      write(mode_label,'(a)') 'order'
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_m, fname_rms, mode_label)
!
      do lm = 0, l_truncation
        write(id_file_rms_m,'(i10,1pe23.14e3,i10,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_m(lm,1:ntot_rms_rj)
      end do
      close(id_file_rms_m)
!
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_vol), '_lm.dat'
      write(mode_label,'(a)') 'diff_deg_order'
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_lm, fname_rms, mode_label)
!
      do lm = 0, l_truncation
        write(id_file_rms_lm,'(i10,1pe23.14e3,i10,1p200e23.14e3)')      &
     &            istep, time, lm, rms_sph_vol_lm(lm,1:ntot_rms_rj)
      end do
      close(id_file_rms_lm)
!
      end subroutine write_sph_vol_ms_spectr_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine write_sph_layer_ms_file(my_rank, istep, time)
!
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: kg, lm, kst
!
!
      if(my_rank .ne. 0)  return
      if(iflag_layer_rms_spec .eq. 0)  return
      if(ntot_rms_rj .eq. 0)  return
!
      kst = 1
      if(iflag_shell_mode .eq. iflag_MESH_same) kst = 0
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id'
      call open_sph_vol_rms_file(id_file_rms, fname_rms, mode_label)
!
      do kg = 1, nidx_rj(1)
        write(id_file_rms,'(i10,1pe23.14e3,i10,1p200e23.14e3)')         &
     &                   istep, time, kg, rms_sph(kg,1:ntot_rms_rj)
      end do
!
      close(id_file_rms)
!
!
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
      write(mode_label,'(a)') 'radial_id    degree'
      call open_sph_vol_rms_file(id_file_rms_l, fname_rms, mode_label)
!
      do kg = 1, nidx_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_l,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_l(kg,lm,1:ntot_rms_rj)
         end do
      end do
      close(id_file_rms_l)
!
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
      write(mode_label,'(a)') 'radial_id    order'
      call open_sph_vol_rms_file(id_file_rms_m, fname_rms, mode_label)
!
      do kg = 1, nidx_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_m,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_m(kg,lm,1:ntot_rms_rj)
         end do
      end do
      close(id_file_rms_m)
!
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
      write(mode_label,'(a)') 'radial_id    diff_deg_order'
      call open_sph_vol_rms_file(id_file_rms_lm, fname_rms, mode_label)
!
      do kg = 1, nidx_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_lm,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')   &
     &           istep, time, kg, lm, rms_sph_lm(kg,lm,1:ntot_rms_rj)
         end do
      end do
      close(id_file_rms_lm)
!
      end subroutine write_sph_layer_ms_file
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine open_sph_vol_rms_file(id_file, fname_rms, mode_label)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_rms_header(id_file, mode_label)
!
      end subroutine open_sph_vol_rms_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_rms_header(id_file, mode_label)
!
      use m_phys_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint) :: i, nri
!
      character(len=kchara) :: labels(6)
!
!
      nri = nidx_rj(1)
      if(iflag_rj_center .gt. 0) nri = nri + 1
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i10)') nri, l_truncation
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(3i10)') nlayer_ICB, nlayer_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i10)')   num_rms_rj, ntot_rms_rj
      write(id_file,'(16i5)')   num_rms_comp_rj(1:num_rms_rj)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, num_rms_rj
        call set_sph_rms_labels(num_rms_comp_rj(i), rms_name_rj(i),     &
     &      labels(1))
        call write_multi_labels(id_file, num_rms_comp_rj(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_rms_header
!
!  --------------------------------------------------------------------
!
      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: num_rms_comp
      character(len = kchara), intent(in) :: rms_name
!
      character(len = kchara), intent(inout) :: labels(num_rms_comp)
!
!
      if ( rms_name .eq. fhd_velo) then
        write(labels(1),'(a)')   'K_ene_pol'
        write(labels(2),'(a)')   'K_ene_tor'
        write(labels(3),'(a)')   'K_ene'
!
      else if (rms_name .eq. fhd_magne) then
        write(labels(1),'(a)')   'M_ene_pol'
        write(labels(2),'(a)')   'M_ene_tor'
        write(labels(3),'(a)')   'M_ene'
!
      else if (rms_name .eq. fhd_filter_v) then
        write(labels(1),'(a)')   'filter_KE_pol'
        write(labels(2),'(a)')   'filter_KE_tor'
        write(labels(3),'(a)')   'filter_KE'
!
      else if (rms_name .eq. fhd_filter_b) then
        write(labels(1),'(a)')   'filter_ME_pol'
        write(labels(2),'(a)')   'filter_ME_tor'
        write(labels(3),'(a)')   'filter_ME'
!
      else if (num_rms_comp .eq. 1) then
        write(labels(1),'(a)')   trim(rms_name)
      else if (num_rms_comp .eq. 3) then
        call add_vector_power_sph_label(rms_name,                       &
     &          labels(1), labels(2), labels(3))
      else if (num_rms_comp .eq. 6) then
        call add_tensor_direction_label_rtp(rms_name,                   &
     &          labels(1), labels(2), labels(3), labels(4), labels(5),  &
     &          labels(6))
      end if
!
      end subroutine set_sph_rms_labels
!
!  --------------------------------------------------------------------
!
      end module output_sph_m_square_file
