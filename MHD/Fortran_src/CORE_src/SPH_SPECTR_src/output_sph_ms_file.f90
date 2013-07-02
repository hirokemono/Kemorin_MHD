!
!      module output_sph_ms_file
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine write_sph_vol_ave_file(my_rank, istep, time)
!      subroutine write_sph_vol_ms_file(my_rank, istep, time)
!      subroutine write_sph_vol_ms_spectr_file(my_rank, istep, time)
!      subroutine write_sph_layer_ms_file(my_rank, istep, time)
!
      module output_sph_ms_file
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
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_vol_ave_file(my_rank, istep, time)
!
      use set_parallel_file_name
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(iflag_volume_ave_sph .eq. 0 .or. my_rank .ne. 0)  return
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
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
!
!
      if(my_rank .ne. 0)  return
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
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: lm
!
!
      if(iflag_volume_rms_spec .eq. 0 .or. my_rank .ne. 0) return
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_l.dat'
      write(mode_label,'(a)') 'degree, '
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_vol), '_m.dat'
      write(mode_label,'(a)') 'order, '
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_m, fname_rms, mode_label)
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_vol), '_lm.dat'
      write(mode_label,'(a)') 'diff_deg_order, '
      call open_sph_vol_rms_file                                        &
     &      (id_file_rms_lm, fname_rms, mode_label)
!
      do lm = 0, l_truncation
        write(id_file_rms_l,'(i10,1pe23.14e3,i10,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_l(1:ntot_rms_rj,lm)
        write(id_file_rms_m,'(i10,1pe23.14e3,i10,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_m(1:ntot_rms_rj,lm)
        write(id_file_rms_lm,'(i10,1pe23.14e3,i10,1p200e23.14e3)')      &
     &            istep, time, lm, rms_sph_vol_lm(1:ntot_rms_rj,lm)
      end do
!
      close(id_file_rms_l)
      close(id_file_rms_m)
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
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      character(len=kchara) :: fname_rms, mode_label
      integer(kind = kint) :: kg, lm
!
!
      if(iflag_layer_rms_spec.eq.0 .or. my_rank .ne. 0) return
!
      write(fname_rms,   '(a,a4)') trim(fhead_rms_layer), '.dat'
      write(mode_label,'(a)') 'radial_id, '
      call open_sph_vol_rms_file(id_file_rms, fname_rms, mode_label)
!
      do kg = 1, nidx_global_rj(1)
        write(id_file_rms,'(i10,1pe23.14e3,i10,1p200e23.14e3)')         &
     &                   istep, time, kg, rms_sph(1:ntot_rms_rj,kg)
      end do
!
      close(id_file_rms)
!
!
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_l.dat'
      write(mode_label,'(a)') 'radial_id, degree, '
      call open_sph_vol_rms_file(id_file_rms_l, fname_rms, mode_label)
!
      write(fname_rms, '(a,a6)') trim(fhead_rms_layer), '_m.dat'
      write(mode_label,'(a)') 'radial_id, order, '
      call open_sph_vol_rms_file(id_file_rms_m, fname_rms, mode_label)
!
      write(fname_rms,'(a,a7)') trim(fhead_rms_layer), '_lm.dat'
      write(mode_label,'(a)') 'radial_id, diff_deg_order, '
      call open_sph_vol_rms_file(id_file_rms_lm, fname_rms, mode_label)
!
      do kg = 1, nidx_global_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_l,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_l(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_m,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_m(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_lm,'(i10,1pe23.14e3,2i10,1p200e23.14e3)')   &
     &           istep, time, kg, lm, rms_sph_lm(1:ntot_rms_rj,lm,kg)
         end do
      end do
!
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      end subroutine write_sph_layer_ms_file
!
!  --------------------------------------------------------------------
!
      end module output_sph_ms_file
