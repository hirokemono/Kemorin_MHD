!
!     module chenge_step_4_dynamic
!
!        Written by H. Matsui on Aug., 2007
!
!      subroutine s_chenge_step_4_dynamic(my_rank)
!      subroutine copy_model_coef_2_previous
!
      module chenge_step_4_dynamic
!
      use m_precision
!
      use m_control_parameter
      use m_t_step_parameter
      use m_SGS_model_coefs
      use m_layering_ele_list
      use m_ele_info_4_dynamical
!
      implicit none
!
      integer(kind=kint), parameter :: sgs_diff_max_code =   30
      character(len=kchara), parameter                                  &
     &           :: sgs_diff_max_name = "SGS_step_monitor.dat"
!
      private :: sgs_diff_max_code, sgs_diff_max_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine open_sgs_diff_monitor(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank .gt. 0) return
!
      open (sgs_diff_max_code,file = sgs_diff_max_name,                 &
     &        status='old', position='append', err = 99)
      return
!
  99  continue
      open (sgs_diff_max_code,file = sgs_diff_max_name,                 &
     &        status='replace')
!
      end subroutine open_sgs_diff_monitor
!
!-----------------------------------------------------------------------
!
      subroutine s_chenge_step_4_dynamic(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint) :: iflag, i, j
      real(kind = kreal) :: diff_r, diff_max
!
!
      iflag = mod(i_step_MHD, i_step_sgs_coefs)
      if (iflag .eq.0 ) then
        call open_sgs_diff_monitor(my_rank)
!
        diff_r = 0.0d0
        diff_max = 0.0d0
        do j = 1, num_sgs_kinds
          do i = 1, n_layer_d
            diff_r = abs(sgs_f_coef(i,j) - coef_sgs_p(i,j))
            diff_max = max(diff_max, diff_r)
          end do
        end do
!
        if (iflag_commute_linear .gt. id_SGS_commute_OFF) then
          if (iset_DIFF_model_coefs .eq. 1) then
            do j = 1, num_diff_kinds
              do i = 1, n_layer_d
                diff_r = abs(diff_f_coef(i,j) - coef_diff_p(i,j))
                diff_max = max(diff_max, diff_r)
              end do
            end do
         else
            do j = 1, num_diff_kinds
                diff_r = abs(diff_f_whole(j) - coef_diff_wp(j))
                diff_max = max(diff_max, diff_r)
            end do
          end if
        end if
!
        call copy_model_coef_2_previous
!
        if (my_rank .eq. 0) write(sgs_diff_max_code,*)                  &
     &    'difference from previous step: ', i_step_MHD, diff_max
!
        if (diff_max .gt. delta_to_shrink_dynamic) then
          if      (i_step_sgs_coefs .eq. 1) then
            i_step_sgs_coefs = 1
          else if (i_step_sgs_coefs .eq. 2) then
            i_step_sgs_coefs = 1
          else if (i_step_sgs_coefs .eq. 5) then
            i_step_sgs_coefs = 2
          else if (i_step_sgs_coefs .eq. 10) then
            i_step_sgs_coefs = 5
          else if (i_step_sgs_coefs .eq. 20) then
            i_step_sgs_coefs = 10
          else if (i_step_sgs_coefs .eq. 50) then
           i_step_sgs_coefs = 20
          else
            i_step_sgs_coefs = i_step_sgs_coefs / 2
          end if
!
          if (i_step_sgs_coefs .gt. max_step_dynamic) then
            i_step_sgs_coefs = max_step_dynamic
          else if (i_step_sgs_coefs .lt. min_step_dynamic) then
            i_step_sgs_coefs = min_step_dynamic
          end if
!
          if (my_rank .eq. 0) write(sgs_diff_max_code,*)                &
     &    'chenge step interbal for dynamic to ', i_step_sgs_coefs
        end if
!
!
        if (diff_max .lt. delta_to_extend_dynamic) then
          if      (i_step_sgs_coefs .eq. 1) then
            i_step_sgs_coefs = 2
          else if (i_step_sgs_coefs .eq. 2) then
            i_step_sgs_coefs = 5
          else if (i_step_sgs_coefs .eq. 5) then
            i_step_sgs_coefs = 10
          else if (i_step_sgs_coefs .eq. 10) then
            i_step_sgs_coefs = 20
          else if (i_step_sgs_coefs .eq. 20) then
            i_step_sgs_coefs = 50
          else
            i_step_sgs_coefs = 2*i_step_sgs_coefs
          end if
!
          if (i_step_sgs_coefs .gt. max_step_dynamic) then
            i_step_sgs_coefs = max_step_dynamic
          else if (i_step_sgs_coefs .lt. min_step_dynamic) then
            i_step_sgs_coefs = min_step_dynamic
          end if
!
          if (my_rank .eq. 0) write(sgs_diff_max_code,*)                &
     &    'chenge step interbal for dynamic to ', i_step_sgs_coefs
        end if
!
        if(my_rank .eq. 0) close(sgs_diff_max_code)
      end if
!
      end subroutine s_chenge_step_4_dynamic
!
!-----------------------------------------------------------------------
!
      subroutine copy_model_coef_2_previous
!
      integer(kind = kint) :: i, j
!
!
      do j = 1, num_sgs_kinds
        do i = 1, n_layer_d
          coef_sgs_p(i,j) = sgs_f_coef(i,j)
        end do
      end do
!
      if (iflag_commute_linear .gt. id_SGS_commute_OFF) then
        if (iset_DIFF_model_coefs .eq. 1) then
          do j = 1, num_diff_kinds
            do i = 1, n_layer_d
              coef_diff_p(i,j) = diff_f_coef(i,j)
            end do
          end do
       else
          do j = 1, num_diff_kinds
            coef_diff_wp(j) = diff_f_whole(j)
          end do
        end if
      end if
!
      end subroutine copy_model_coef_2_previous
!
!-----------------------------------------------------------------------
!
      end module chenge_step_4_dynamic
