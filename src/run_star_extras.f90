
      module run_star_extras

      use eos_lib
      use star_lib
      use star_def
      use const_def
      
      implicit none

      integer :: time0, time1, clock_rate
      
      ! these routines are called by the standard run_star check_model
      contains
      
      
      subroutine extras_controls(id, ierr)
         integer, intent(in) :: id
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         
         s% extras_startup => extras_startup
         s% extras_check_model => extras_check_model
         s% extras_finish_step => extras_finish_step
         s% extras_after_evolve => extras_after_evolve
         s% how_many_extra_history_columns => how_many_extra_history_columns
         s% data_for_extra_history_columns => data_for_extra_history_columns
         s% how_many_extra_profile_columns => how_many_extra_profile_columns
         s% data_for_extra_profile_columns => data_for_extra_profile_columns  

         !define other routines here; remember to set use_other_ in inlist
         s% other_kap_get => my_kap_get
         !s% other_kap_get => Kramers_kap_get
         
         s% other_eosDT_get => my_eosDT_get
         s% other_eosDT_get_T => my_eosDT_get_T
         s% other_eosDT_get_Rho => my_eosDT_get_Rho
         end subroutine extras_controls
      
      
      integer function extras_startup(id, restart, ierr)
         integer, intent(in) :: id
         logical, intent(in) :: restart
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_startup = 0
         call system_clock(time0,clock_rate)
         if (.not. restart) then
            call alloc_extra_info(s)
         else ! it is a restart
            call unpack_extra_info(s)
         end if
      end function extras_startup
      
      
      subroutine extras_after_evolve(id, id_extra, ierr)
         integer, intent(in) :: id, id_extra
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         real(dp) :: dt
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         call system_clock(time1,clock_rate)
         dt = dble(time1 - time0) / clock_rate / 60
         write(*,'(/,a50,f12.2,99i10/)') 'runtime (minutes), retries, backups, steps', &
            dt, s% num_retries, s% num_backups, s% model_number
         ierr = 0
      end subroutine extras_after_evolve
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_check_model(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_check_model = keep_going         
      end function extras_check_model


      integer function how_many_extra_history_columns(id, id_extra)
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_history_columns = 0
      end function how_many_extra_history_columns
      
      
      subroutine data_for_extra_history_columns(id, id_extra, n, names, vals, ierr)
         integer, intent(in) :: id, id_extra, n
         character (len=maxlen_history_column_name) :: names(n)
         real(dp) :: vals(n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_history_columns

      
      integer function how_many_extra_profile_columns(id, id_extra)
         use star_def, only: star_info
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         how_many_extra_profile_columns = 0
      end function how_many_extra_profile_columns
      
      
      subroutine data_for_extra_profile_columns(id, id_extra, n, nz, names, vals, ierr)
         use star_def, only: star_info, maxlen_profile_column_name
         use const_def, only: dp
         integer, intent(in) :: id, id_extra, n, nz
         character (len=maxlen_profile_column_name) :: names(n)
         real(dp) :: vals(nz,n)
         integer, intent(out) :: ierr
         type (star_info), pointer :: s
         integer :: k
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
      end subroutine data_for_extra_profile_columns
      

      ! returns either keep_going, retry, backup, or terminate.
      integer function extras_finish_step(id, id_extra)
         use chem_def
         integer, intent(in) :: id, id_extra
         integer :: ierr
         type (star_info), pointer :: s
         ierr = 0
         call star_ptr(id, s, ierr)
         if (ierr /= 0) return
         extras_finish_step = keep_going
         call store_extra_info(s)
      end function extras_finish_step
      
      
      ! routines for saving and restoring extra data so can do restarts
         
         ! put these defs at the top and delete from the following routines
         !integer, parameter :: extra_info_alloc = 1
         !integer, parameter :: extra_info_get = 2
         !integer, parameter :: extra_info_put = 3
      
      
      subroutine alloc_extra_info(s)
         integer, parameter :: extra_info_alloc = 1
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_alloc)
      end subroutine alloc_extra_info
      
      
      subroutine unpack_extra_info(s)
         integer, parameter :: extra_info_get = 2
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_get)
      end subroutine unpack_extra_info
      
      
      subroutine store_extra_info(s)
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         call move_extra_info(s,extra_info_put)
      end subroutine store_extra_info
      
      
      subroutine move_extra_info(s,op)
         integer, parameter :: extra_info_alloc = 1
         integer, parameter :: extra_info_get = 2
         integer, parameter :: extra_info_put = 3
         type (star_info), pointer :: s
         integer, intent(in) :: op
         
         integer :: i, j, num_ints, num_dbls, ierr
         
         i = 0
         ! call move_int or move_flg    
         num_ints = i
         
         i = 0
         ! call move_dbl       
         
         num_dbls = i
         
         if (op /= extra_info_alloc) return
         if (num_ints == 0 .and. num_dbls == 0) return
         
         ierr = 0
         call star_alloc_extras(s% id, num_ints, num_dbls, ierr)
         if (ierr /= 0) then
            write(*,*) 'failed in star_alloc_extras'
            write(*,*) 'alloc_extras num_ints', num_ints
            write(*,*) 'alloc_extras num_dbls', num_dbls
            stop 1
         end if
         
         contains
         
         subroutine move_dbl(dbl)
            real(dp) :: dbl
            i = i+1
            select case (op)
            case (extra_info_get)
               dbl = s% extra_work(i)
            case (extra_info_put)
               s% extra_work(i) = dbl
            end select
         end subroutine move_dbl
         
         subroutine move_int(int)
            integer :: int
            i = i+1
            select case (op)
            case (extra_info_get)
               int = s% extra_iwork(i)
            case (extra_info_put)
               s% extra_iwork(i) = int
            end select
         end subroutine move_int
         
         subroutine move_flg(flg)
            logical :: flg
            i = i+1
            select case (op)
            case (extra_info_get)
               flg = (s% extra_iwork(i) /= 0)
            case (extra_info_put)
               if (flg) then
                  s% extra_iwork(i) = 1
               else
                  s% extra_iwork(i) = 0
               end if
            end select
         end subroutine move_flg
      
      end subroutine move_extra_info


      subroutine my_kap_get( &
            id, k, handle, zbar, X, Z, Zbase, XC, XN, XO, XNe, &
            logRho, logT, species, chem_id, net_iso, xa, &
            lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
            frac_Type2, kap, dlnkap_dlnRho, dlnkap_dlnT, ierr)
 
            use kap_lib, only: kap_get

         ! INPUT
         integer, intent(in) :: id ! star id if available; 0 otherwise
         integer, intent(in) :: k ! cell number or 0 if not for a particular cell         
         integer, intent(in) :: handle ! from alloc_kap_handle
         real(dp), intent(in) :: zbar ! average ion charge
         real(dp), intent(in) :: X, Z, Zbase, XC, XN, XO, XNe ! composition    
         real(dp), intent(in) :: logrho ! density
         real(dp), intent(in) :: logT ! temperature
         real(dp), intent(in) :: lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT
            ! free_e := total combined number per nucleon of free electrons and positrons

         integer, intent(in) :: species
         integer, pointer :: chem_id(:) ! maps species to chem id
            ! index from 1 to species
            ! value is between 1 and num_chem_isos         
         integer, pointer :: net_iso(:) ! maps chem id to species number
            ! index from 1 to num_chem_isos (defined in chem_def)
            ! value is 0 if the iso is not in the current net
            ! else is value between 1 and number of species in current net
         real(dp), intent(in) :: xa(:) ! mass fractions
         
         ! LOCAL
         type (star_info), pointer :: s
         real(dp) :: my_X, my_Z

         ! OUTPUT
         real(dp), intent(out) :: frac_Type2
         real(dp), intent(out) :: kap ! opacity
         real(dp), intent(out) :: dlnkap_dlnRho ! partial derivative at constant T
         real(dp), intent(out) :: dlnkap_dlnT   ! partial derivative at constant Rho
         integer, intent(out) :: ierr ! 0 means AOK.
                  

         call star_ptr(id, s, ierr)
         if (ierr /= 0) return

         if (s% job% extras_rpar(1) < 0.0d0) then 
            my_X = X
         else
            my_X = s% job% extras_rpar(1)
         endif

         if (s% job% extras_rpar(2) < 0.0d0) then
            my_Z = Z
         else
            my_Z = s% job% extras_rpar(2)
         endif

         call kap_get( &
            handle, zbar, my_X, my_Z, Zbase, XC, XN, XO, XNe, logRho, logT, &
            lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
            frac_Type2, kap, dlnkap_dlnRho, dlnkap_dlnT, ierr)

      end subroutine my_kap_get



      subroutine Kramers_kap_get( &
            id, k, handle, zbar, X, Z, Zbase, XC, XN, XO, XNe, &
            logRho, logT, species, chem_id, net_iso, xa, &
            lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT, &
            frac_Type2, kap, dlnkap_dlnRho, dlnkap_dlnT, ierr)
 
            use crlibm_lib, only: log_cr, exp10_cr

         ! INPUT
         integer, intent(in) :: id ! star id if available; 0 otherwise
         integer, intent(in) :: k ! cell number or 0 if not for a particular cell         
         integer, intent(in) :: handle ! from alloc_kap_handle
         real(dp), intent(in) :: zbar ! average ion charge
         real(dp), intent(in) :: X, Z, Zbase, XC, XN, XO, XNe ! composition    
         real(dp), intent(in) :: logrho ! density
         real(dp), intent(in) :: logT ! temperature
         real(dp), intent(in) :: lnfree_e, d_lnfree_e_dlnRho, d_lnfree_e_dlnT
            ! free_e := total combined number per nucleon of free electrons and positrons

         integer, intent(in) :: species
         integer, pointer :: chem_id(:) ! maps species to chem id
            ! index from 1 to species
            ! value is between 1 and num_chem_isos         
         integer, pointer :: net_iso(:) ! maps chem id to species number
            ! index from 1 to num_chem_isos (defined in chem_def)
            ! value is 0 if the iso is not in the current net
            ! else is value between 1 and number of species in current net
         real(dp), intent(in) :: xa(:) ! mass fractions
         
         ! LOCAL
         real(dp) :: lnkappa_bf, lnkappa_ff, dlnkbf_dlnT, dlnkff_dlnT
         real(dp), parameter :: ln_k_bf = 59.0325d0
         real(dp), parameter :: ln_k_ff = 51.9598d0

         ! OUTPUT
         real(dp), intent(out) :: frac_Type2
         real(dp), intent(out) :: kap ! opacity
         real(dp), intent(out) :: dlnkap_dlnRho ! partial derivative at constant T
         real(dp), intent(out) :: dlnkap_dlnT   ! partial derivative at constant Rho
         integer, intent(out) :: ierr ! 0 means AOK.

         frac_Type2 = 0.0d0
         ierr = 0

         lnkappa_bf = ln_k_bf + log_cr(          Z * (1.0d0 + X)) + logRho - 3.5d0*logT
         lnkappa_ff = ln_k_ff + log_cr((1.0d0 - Z) * (1.0d0 + X)) + logRho - 3.5d0*logT

         kap = exp10_cr(lnkappa_bf) + exp10_cr(lnkappa_ff)
         dlnkap_dlnRho = 1.0d0
         dlnKap_dlnT = -3.5d0

      end subroutine Kramers_kap_get


      subroutine my_eosDT_get( &
              id, k, handle, Z, X, abar, zbar, & 
              species, chem_id, net_iso, xa, &
              Rho, log10Rho, T, log10T, & 
              res, d_dlnRho_const_T, d_dlnT_const_Rho, &
              d_dabar_const_TRho, d_dzbar_const_TRho, ierr)

         ! INPUT
         use chem_def, only: num_chem_isos
         
         integer, intent(in) :: id ! star id if available; 0 otherwise
         integer, intent(in) :: k ! cell number or 0 if not for a particular cell         
         integer, intent(in) :: handle ! eos handle

         real(dp), intent(in) :: Z ! the metals mass fraction
         real(dp), intent(in) :: X ! the hydrogen mass fraction
            
         real(dp), intent(in) :: abar
            ! mean atomic number (nucleons per nucleus; grams per mole)
         real(dp), intent(in) :: zbar ! mean charge per nucleus
         
         integer, intent(in) :: species
         integer, pointer :: chem_id(:) ! maps species to chem id
            ! index from 1 to species
            ! value is between 1 and num_chem_isos         
         integer, pointer :: net_iso(:) ! maps chem id to species number
            ! index from 1 to num_chem_isos (defined in chem_def)
            ! value is 0 if the iso is not in the current net
            ! else is value between 1 and number of species in current net
         real(dp), intent(in) :: xa(:) ! mass fractions
         
         real(dp), intent(in) :: Rho, log10Rho ! the density
            ! provide both if you have them.  else pass one and set the other to arg_not_provided
            ! "arg_not_provided" is defined in mesa const_def
            
         real(dp), intent(in) :: T, log10T ! the temperature
            ! provide both if you have them.  else pass one and set the other to arg_not_provided
                     
         ! OUTPUT
         
         real(dp), intent(inout) :: res(:) ! (num_eos_basic_results)
         ! partial derivatives of the basic results wrt lnd and lnT
         real(dp), intent(inout) :: d_dlnRho_const_T(:) ! (num_eos_basic_results)  
         ! d_dlnRho_c_T(i) = d(res(i))/dlnd|T
         real(dp), intent(inout) :: d_dlnT_const_Rho(:) ! (num_eos_basic_results) 
         ! d_dlnT(i) = d(res(i))/dlnT|Rho
         real(dp), intent(inout) :: d_dabar_const_TRho(:) ! (num_eos_basic_results) 
         real(dp), intent(inout) :: d_dzbar_const_TRho(:) ! (num_eos_basic_results) 
         
         integer, intent(out) :: ierr ! 0 means AOK.

         !LOCAL
         real(dp) :: my_X

         my_X = 1.0d0 - Z

         call eosDT_get( &
               handle, Z, X, abar, zbar, &
               species, chem_id, net_iso, xa, &
               Rho, log10Rho, T, log10T, &
               res, d_dlnRho_const_T, d_dlnT_const_Rho, &
               d_dabar_const_TRho, d_dzbar_const_TRho, ierr)
         
      end subroutine my_eosDT_get
      
      subroutine my_eosDT_get_T( &
               id, k, handle, Z, X, abar, zbar, &
               species, chem_id, net_iso, xa, &
               logRho, which_other, other_value, &
               logT_tol, other_tol, max_iter, logT_guess, & 
               logT_bnd1, logT_bnd2, other_at_bnd1, other_at_bnd2, &
               logT_result, res, d_dlnRho_const_T, d_dlnT_const_Rho, &
               d_dabar_const_TRho, d_dzbar_const_TRho, eos_calls, ierr)
     
         ! finds log10 T given values for density and 'other', and initial guess for temperature.
         ! does up to max_iter attempts until logT changes by less than tol.
         
         ! 'other' can be any of the basic result variables for the eos
         ! specify 'which_other' by means of the definitions in eos_def (e.g., i_lnE)
         
         use chem_def, only: num_chem_isos

         integer, intent(in) :: id ! star id if available; 0 otherwise
         integer, intent(in) :: k ! cell number or 0 if not for a particular cell         
         integer, intent(in) :: handle

         real(dp), intent(in) :: Z ! the metals mass fraction
         real(dp), intent(in) :: X ! the hydrogen mass fraction
            
         real(dp), intent(in) :: abar
            ! mean atomic number (nucleons per nucleus; grams per mole)
         real(dp), intent(in) :: zbar ! mean charge per nucleus
         
         integer, intent(in) :: species
         integer, pointer :: chem_id(:) ! maps species to chem id
            ! index from 1 to species
            ! value is between 1 and num_chem_isos         
         integer, pointer :: net_iso(:) ! maps chem id to species number
            ! index from 1 to num_chem_isos (defined in chem_def)
            ! value is 0 if the iso is not in the current net
            ! else is value between 1 and number of species in current net
         real(dp), intent(in) :: xa(:) ! mass fractions
         
         real(dp), intent(in) :: logRho ! log10 of density
         integer, intent(in) :: which_other ! from eos_def.  e.g., i_lnE
         real(dp), intent(in) :: other_value ! desired value for the other variable
         real(dp), intent(in) :: other_tol
         
         real(dp), intent(in) :: logT_tol
         integer, intent(in) :: max_iter ! max number of iterations        

         real(dp), intent(in) :: logT_guess ! log10 of temperature
         real(dp), intent(in) :: logT_bnd1, logT_bnd2 ! bounds for logT
            ! if don't know bounds, just set to arg_not_provided (defined in const_def)
         real(dp), intent(in) :: other_at_bnd1, other_at_bnd2 ! values at bounds
            ! if don't know these values, just set to arg_not_provided (defined in const_def)

         real(dp), intent(out) :: logT_result ! log10 of temperature
         real(dp), intent(inout) :: res(:) ! (num_eos_basic_results)
         real(dp), intent(inout) :: d_dlnRho_const_T(:) ! (num_eos_basic_results) 
         real(dp), intent(inout) :: d_dlnT_const_Rho(:) ! (num_eos_basic_results)
         real(dp), intent(inout) :: d_dabar_const_TRho(:) ! (num_eos_basic_results) 
         real(dp), intent(inout) :: d_dzbar_const_TRho(:) ! (num_eos_basic_results) 
         
         integer, intent(out) :: eos_calls
         integer, intent(out) :: ierr ! 0 means AOK.

         ! LOCAL
         real(dp) :: my_X

         my_X = 1.0d0 - Z
         
         call eosDT_get_T( &
               handle, Z, my_X, abar, zbar, &
               species, chem_id, net_iso, xa, &
               logRho, which_other, other_value, &
               logT_tol, other_tol, max_iter, logT_guess, &
               logT_bnd1, logT_bnd2, other_at_bnd1, other_at_bnd2, &
               logT_result, res, d_dlnRho_const_T, d_dlnT_const_Rho, &
               d_dabar_const_TRho, d_dzbar_const_TRho, eos_calls, ierr)

      end subroutine my_eosDT_get_T
      

      subroutine my_eosDT_get_Rho( &
               id, k, handle, Z, X, abar, zbar, &
               species, chem_id, net_iso, xa, &
               logT, which_other, other_value, &
               logRho_tol, other_tol, max_iter, logRho_guess,  &
               logRho_bnd1, logRho_bnd2, other_at_bnd1, other_at_bnd2, &
               logRho_result, res, d_dlnRho_const_T, d_dlnT_const_Rho, &
               d_dabar_const_TRho, d_dzbar_const_TRho, eos_calls, ierr)
     
         ! finds log10 Rho given values for temperature and 'other', and initial guess for density.
         ! does up to max_iter attempts until logRho changes by less than tol.
         
         ! 'other' can be any of the basic result variables for the eos
         ! specify 'which_other' by means of the definitions in eos_def (e.g., i_lnE)
         
         use chem_def, only: num_chem_isos
         
         integer, intent(in) :: id ! star id if available; 0 otherwise
         integer, intent(in) :: k ! cell number or 0 if not for a particular cell         
         integer, intent(in) :: handle

         real(dp), intent(in) :: Z ! the metals mass fraction
         real(dp), intent(in) :: X ! the hydrogen mass fraction
            
         real(dp), intent(in) :: abar
            ! mean atomic number (nucleons per nucleus; grams per mole)
         real(dp), intent(in) :: zbar ! mean charge per nucleus
         
         integer, intent(in) :: species
         integer, pointer :: chem_id(:) ! maps species to chem id
            ! index from 1 to species
            ! value is between 1 and num_chem_isos         
         integer, pointer :: net_iso(:) ! maps chem id to species number
            ! index from 1 to num_chem_isos (defined in chem_def)
            ! value is 0 if the iso is not in the current net
            ! else is value between 1 and number of species in current net
         real(dp), intent(in) :: xa(:) ! mass fractions
         
         real(dp), intent(in) :: logT ! log10 of temperature

         integer, intent(in) :: which_other ! from eos_def.  e.g., i_lnE
         real(dp), intent(in) :: other_value ! desired value for the other variable
         real(dp), intent(in) :: other_tol
         
         real(dp), intent(in) :: logRho_tol

         integer, intent(in) :: max_iter ! max number of Newton iterations        

         real(dp), intent(in) :: logRho_guess ! log10 of density
         real(dp), intent(in) :: logRho_bnd1, logRho_bnd2 ! bounds for logRho
            ! if don't know bounds, just set to arg_not_provided (defined in const_def)
         real(dp), intent(in) :: other_at_bnd1, other_at_bnd2 ! values at bounds
            ! if don't know these values, just set to arg_not_provided (defined in const_def)

         real(dp), intent(out) :: logRho_result ! log10 of density

         real(dp), intent(inout) :: res(:) ! (num_eos_basic_results)
         real(dp), intent(inout) :: d_dlnRho_const_T(:) ! (num_eos_basic_results) 
         real(dp), intent(inout) :: d_dlnT_const_Rho(:) ! (num_eos_basic_results)
         real(dp), intent(inout) :: d_dabar_const_TRho(:) ! (num_eos_basic_results) 
         real(dp), intent(inout) :: d_dzbar_const_TRho(:) ! (num_eos_basic_results) 

         integer, intent(out) :: eos_calls
         integer, intent(out) :: ierr ! 0 means AOK.

         ! LOCAL
         real(dp) :: my_X

         my_X = 1.0d0 - Z
         
         call eosDT_get_Rho( &
              handle, Z, my_X, abar, zbar, &
              species, chem_id, net_iso, xa, &
              logT, which_other, other_value, &
              logRho_tol, other_tol, max_iter, logRho_guess, &
              logRho_bnd1, logRho_bnd2, other_at_bnd1, other_at_bnd2, &
              logRho_result, res, d_dlnRho_const_T, d_dlnT_const_Rho, &
              d_dabar_const_TRho, d_dzbar_const_TRho, eos_calls, ierr)
         
      end subroutine my_eosDT_get_Rho

      end module run_star_extras
      
