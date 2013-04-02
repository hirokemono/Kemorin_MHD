!output_sph_rms_one_layer.f90
!      module output_sph_rms_one_layer
!
!     Written by H. Matsui on Feb., 2008
!
!      subroutine open_sph_rms_one_layer_file(my_rank)
!      subroutine close_sph_rms_one_layer_file(my_rank)
!      subroutine write_sph_picked_layer_rms_data(my_rank, istep, time)
!
      module output_sph_rms_one_layer
!
      use m_precision
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_rms_4_sph_spectr
      use m_ctl_params_sph_utils
      use m_pickup_sph_spectr_data
!
      implicit none
!
!  --------------------------------------------------------------------
!
      contains
!
!  --------------------------------------------------------------------
!
      subroutine open_sph_rms_one_layer_file(my_rank)
!
      use m_sph_phys_address
      use set_parallel_file_name
      use output_sph_rms_data
!
      integer(kind = kint), intent(in) :: my_rank
!
      character(len = kchara) :: fname_tmp1, fname_tmp2
      character(len = kchara) :: fname_rms_l,  fname_rms_m
      character(len = kchara) :: fname_rms_lm, fname_rms
!
!
      if(my_rank .ne. 0) return
!
      write(fname_rms_l, '(a,a2)') trim(fhead_rms_layer), '_l'
      call add_int_suffix(id_pick_layer(1), fname_rms_l, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms_l)
!
      write(fname_rms_m, '(a,a2)') trim(fhead_rms_layer), '_m'
      call add_int_suffix(id_pick_layer(1), fname_rms_m, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms_m)
!
      write(fname_rms_lm, '(a,a3)') trim(fhead_rms_layer), '_lm'
      call add_int_suffix(id_pick_layer(1), fname_rms_lm, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms_lm)
!
      write(fname_rms, '(a,a3)') trim(fhead_rms_layer), '_lm'
      call add_int_suffix(id_pick_layer(1), fname_rms, fname_tmp1)
      call add_int_suffix(id_pick_layer(num_pick_layer), fname_tmp1,    &
     &    fname_tmp2)
      call add_dat_extension(fname_tmp2, fname_rms)
!
      open(id_file_rms_l,    file=fname_rms_l,    form='formatted')
      open(id_file_rms_m,    file=fname_rms_m,    form='formatted')
      open(id_file_rms_lm,   file=fname_rms_lm,   form='formatted')
!
      open(id_file_rms1,      file=fname_rms,      form='formatted')
!
      call write_sph_rms_layer_head(id_file_rms1, id_file_rms_l,        &
     &      id_file_rms_m, id_file_rms_lm)
!
      end subroutine open_sph_rms_one_layer_file
!
!  --------------------------------------------------------------------
!
      subroutine close_sph_rms_one_layer_file(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank .ne. 0) return
      close(id_file_rms_l)
      close(id_file_rms_m)
      close(id_file_rms_lm)
!
      close(id_file_rms1)
!
      end subroutine close_sph_rms_one_layer_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_picked_layer_rms_data(my_rank, istep, time)
!
      integer(kind = kint), intent(in) :: my_rank, istep
      real(kind = kreal), intent(in) :: time
      integer(kind = kint) :: kg, lm, k
!
!
      if (my_rank .ne. 0) return
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        do lm = 0, l_truncation
          write(id_file_rms_l,                                          &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_l(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_m,                                          &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_m(1:ntot_rms_rj,lm,kg)
          write(id_file_rms_lm,                                         &
     &                  '(i10,1pe23.14e3,2i10,1p200e23.14e3)')          &
     &           istep, time, kg, lm, rms_sph_lm(1:ntot_rms_rj,lm,kg)
        end do
      end do
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        write(id_file_rms1,'(i10,1pe23.14e3,i10,1p200e23.14e3)')        &
     &                   istep, time, kg, rms_sph(1:ntot_rms_rj,kg)
      end do
!
      end subroutine write_sph_picked_layer_rms_data
!
!  --------------------------------------------------------------------
!
      end module output_sph_rms_one_layer
